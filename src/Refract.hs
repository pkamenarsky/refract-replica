{-# LANGUAGE OverloadedStrings #-}

module Refract where

import           Control.Applicative ((<|>))

import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Control.Concurrent.STM (atomically, retry)
import           Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)

import           Data.Foldable (asum)
import qualified Data.Text as T

import qualified Replica.VDOM as V
import qualified Replica.VDOM.Types as V

import qualified Refract.DOM as Refract
import           Refract.DOM (Component (Component, runComponent))

import           Network.WebSockets.Connection (ConnectionOptions, defaultConnectionOptions)
import           Network.Wai (Middleware)
import qualified Network.Wai.Handler.Replica as R
import qualified Network.Wai.Handler.Warp as W

state :: (st -> Component st) -> Component st
state f = Component $ \path setState st -> runComponent (f st) path setState st

state' :: (st -> (st -> IO ()) -> Component st) -> Component st
state' f = Component $ \path setState st -> runComponent (f st setState) path setState st

type Bounds = (Double, Double, Double, Double)

type GetBounds = Refract.DomPath -> IO (Double, Double, Double, Double)

getBounds :: R.Context -> GetBounds
getBounds ctx path = do
  rect <- newEmptyMVar
  cb <- R.registerCallback ctx (putMVar rect)
  R.call ctx (cb, reverse path) js
  takeMVar rect
  where
    js = undefined
      "let element = document.body; \n\
    \ for (let i = 0; i < arg[1].length; i++) { \n\
    \  element = element.childNodes[arg[1][i]]; \n\
    \ let rect = element.getBoundingClientRect(); \n\
    \ callCallback(arg[0], [rect.left, rect.top, rect.right - rect.left, rect.bottom - rect.top]);"

--------------------------------------------------------------------------------

run :: Int -> V.HTML -> ConnectionOptions -> Middleware -> (st -> IO ()) -> IO st -> IO [TChan (st -> IO st)] -> (R.Context -> [TChan (st -> IO st)] -> Component st) -> IO ()
run port index connectionOptions middleware setState st exModStChs component = do
  cmpStCh <- newTChanIO
  W.run port $ R.app index connectionOptions middleware st ((,) <$> newTChanIO <*> exModStChs) (step setState component)

runDefault :: Int -> T.Text -> (st -> IO ()) -> IO st -> IO [TChan (st -> IO st)] -> (R.Context -> [TChan (st -> IO st)] -> Component st) -> IO ()
runDefault port title setState st exModStChs component = do
  W.run port $ R.app (V.defaultIndex title []) defaultConnectionOptions id st ((,) <$> newTChanIO <*> exModStChs) (step setState component)

step :: (st -> IO ()) -> (R.Context -> [TChan (st -> IO st)] -> Component st) -> R.Context -> st -> (TChan st, [TChan (st -> IO st)]) -> IO (V.HTML, R.Event -> Maybe (IO ()), IO (Maybe st))
step setState f ctx st (cmpStCh, exModStChs) = do
  let html = Refract.runComponent (f ctx exModStChs) [0] (atomically . writeTChan cmpStCh) st
  pure
    ( html
    , \event -> V.fireEvent html (R.evtPath event) (R.evtType event) (V.DOMEvent $ R.evtEvent event)
    , Just <$> readSt
    )
  where
    readSt = do
      f <- atomically $ asum
        [ fmap pure . const <$> readTChan cmpStCh
        , asum $ map readTChan exModStChs
        ]
      st' <- f st
      setState st'
      pure st'
