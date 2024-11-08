{-# LANGUAGE OverloadedStrings #-}

module Refract where

import           Control.Applicative ((<|>))

import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Control.Concurrent.STM (atomically, retry)
import           Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)
import           Control.Monad.Trans.Reader as R
import           Control.Monad.Trans.Writer.CPS as W

import qualified Data.Aeson as A
import qualified Data.Map as M
import           Data.Foldable (asum)
import qualified Data.Text as T

import qualified Replica.VDOM as V
import qualified Replica.VDOM.Types as V

import qualified Refract.DOM as Refract
import           Refract.DOM (UI, UI' (UI))

import           Network.WebSockets.Connection (ConnectionOptions, defaultConnectionOptions)
import           Network.Wai (Middleware)
import qualified Network.Wai.Handler.Replica as R
import qualified Network.Wai.Handler.Warp as W

state :: (st -> UI st) -> UI st
state f = UI $ do
  UI ui <- R.asks $ \(_, st) -> f st
  ui

state' :: (st -> (st -> IO ()) -> UI st) -> UI st
state' f = UI $ do
  UI ui <- R.asks $ \(setState, st) -> f st setState
  ui

--------------------------------------------------------------------------------

run
  :: Int
  -> V.HTML
  -> ConnectionOptions
  -> Middleware
  -> (st -> IO ())
  -> IO st
  -> (R.Context -> IO [TChan (st -> IO st)])
  -> (R.Context -> [TChan (st -> IO st)] -> UI st)
  -> IO ()
run port index connectionOptions middleware setState st getExModStChs component = do
  W.run port $ R.app index connectionOptions middleware st getSession (step setState component)
  where
    getSession ctx = do
      cmpStCh <- newTChanIO
      exModStChs <- getExModStChs ctx
      pure (cmpStCh, exModStChs)

runDefault
  :: Int
  -> T.Text
  -> (st -> IO ())
  -> IO st
  -> (R.Context -> IO [TChan (st -> IO st)])
  -> (R.Context -> [TChan (st -> IO st)] -> UI st)
  -> IO ()
runDefault port title setState st getExModStChs component = do
  W.run port $ R.app (V.defaultIndex title []) defaultConnectionOptions id st getSession (step setState component)
  where
    getSession ctx = do
      cmpStCh <- newTChanIO
      exModStChs <- getExModStChs ctx
      pure (cmpStCh, exModStChs)

step
  :: (st -> IO ())
  -> (R.Context -> [TChan (st -> IO st)] -> UI st)
  -> R.Context
  -> st
  -> (TChan st, [TChan (st -> IO st)])
  -> IO (V.HTML, R.Event -> Maybe (IO ()), IO (Maybe st))
step setState f ctx st (cmpStCh, exModStChs) = do
  let UI ui = f ctx exModStChs
      html = W.execWriter $ R.runReaderT ui (atomically . writeTChan cmpStCh, st)
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
