{-# LANGUAGE OverloadedStrings #-}

module Refract where

import qualified Control.Concurrent.MVar as MVar

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
state f = Component $ \setState st -> runComponent (f st) setState st

state' :: (st -> (st -> IO ()) -> Component st) -> Component st
state' f = Component $ \setState st -> runComponent (f st setState) setState st

--------------------------------------------------------------------------------

run :: Int -> V.HTML -> ConnectionOptions -> Middleware -> st -> (R.Context -> IO (Component st)) -> IO ()
run port index connectionOptions middleware st component
  = W.run port
  $ R.app index connectionOptions middleware st (step component)

runDefault :: Int -> T.Text -> st -> (R.Context -> IO (Component st)) -> IO ()
runDefault port title st component
  = W.run port
  $ R.app (V.defaultIndex title []) defaultConnectionOptions id st (step component)

step :: (R.Context -> IO (Component st)) -> R.Context -> st -> IO (V.HTML, R.Event -> Maybe (IO ()), IO (Maybe st))
step f ctx st = do
  stRef <- MVar.newEmptyMVar
  cmp <- f ctx
  let html = Refract.runComponent cmp (MVar.putMVar stRef) st
  pure
    ( html
    , \event -> V.fireEvent html (R.evtPath event) (R.evtType event) (V.DOMEvent $ R.evtEvent event)
    , Just <$> MVar.takeMVar stRef
    )
