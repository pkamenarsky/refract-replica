{-# LANGUAGE OverloadedStrings #-}

module Refract where

import qualified Control.Concurrent.MVar as MVar
import Control.Monad.Fix (mfix)

import qualified Data.Text as T

import qualified Network.Wai.Handler.Replica as Replica
import qualified Replica.VDOM as V
import qualified Replica.VDOM.Types as V

import qualified Refract.DOM as Refract
import           Refract.DOM (Component (Component, runComponent))

import           Network.WebSockets.Connection   (ConnectionOptions, defaultConnectionOptions)
import           Network.Wai                     (Middleware)
import qualified Network.Wai.Handler.Replica     as R
import qualified Network.Wai.Handler.Warp        as W

state :: (st -> Component st) -> Component st
state f = Component $ \setState st -> runComponent (f st) setState st

--------------------------------------------------------------------------------

registerDragAndDrop :: R.Context -> (Maybe (Int, Int) -> IO ()) -> IO ()
registerDragAndDrop ctx cb = do
  jsCb <- mfix $ \jsCb -> do
    jsCb <- R.registerCallback ctx (dragged jsCb)
    pure jsCb

  R.call ctx jsCb js
  where
    js = "var down = function(e) { \n\
           \ e.preventDefault(); \n\
           \ var drag = function(f) { \n\
           \   callCallback(arg, [f.clientX, f.clientY]); \n\
           \ }; \n\
           \ window.addEventListener('mousemove', drag); \n\
           \ var up = function() { \n\
           \   window.removeEventListener('mousemove', drag); \n\
           \   window.removeEventListener('mousedown', down); \n\
           \   window.removeEventListener('mouseup', up); \n\
           \   callCallback(arg, null); \n\
           \ }; \n\
           \ window.addEventListener('mouseup', up); \n\
         \}; \n\
         \window.addEventListener('mousedown', down); \n\
         \"

    dragged :: R.Callback -> Maybe (Int, Int) -> IO ()
    dragged jsCb xy@Nothing = do
      R.unregisterCallback ctx jsCb
      cb xy
    dragged jsCb xy = cb xy

dragAndDrop :: R.Context -> (Maybe (Int, Int) -> IO ()) -> IO ()
dragAndDrop ctx cb = do
  jsCb <- mfix $ \jsCb -> do
    jsCb <- R.registerCallback ctx (dragged jsCb)
    pure jsCb

  R.call ctx jsCb js
  where
    js = "var drag = function(f) { \n\
        \   callCallback(arg, [f.clientX, f.clientY]); \n\
        \ }; \n\
        \ var up = function() { \n\
        \   window.removeEventListener('mousemove', drag); \n\
        \   window.removeEventListener('mousedown', down); \n\
        \   window.removeEventListener('mouseup', up); \n\
        \   callCallback(arg, null); \n\
        \ }; \n\
        \ window.addEventListener('mousemove', drag); \n\
        \ window.addEventListener('mouseup', up); \n\
      \}; \n\
      \"

    dragged :: R.Callback -> Maybe (Int, Int) -> IO ()
    dragged jsCb xy@Nothing = do
      R.unregisterCallback ctx jsCb
      cb xy
    dragged jsCb xy = cb xy

--------------------------------------------------------------------------------


run :: Int -> V.HTML -> ConnectionOptions -> Middleware -> st -> (R.Context -> IO (Component st)) -> IO ()
run port index connectionOptions middleware st component
  = W.run port
  $ R.app index connectionOptions middleware st (step component)

runDefault :: Int -> T.Text -> st -> (R.Context -> IO (Component st)) -> IO ()
runDefault port title st component
  = W.run port
  $ R.app (V.defaultIndex title []) defaultConnectionOptions id st (step component)

step :: (Replica.Context -> IO (Component st)) -> Replica.Context -> st -> IO (V.HTML, Replica.Event -> Maybe (IO ()), IO (Maybe st))
step f ctx st = do
  stRef <- MVar.newEmptyMVar
  cmp <- f ctx
  let html = Refract.runComponent cmp (MVar.putMVar stRef) st
  pure
    ( html
    , \event -> V.fireEvent html (Replica.evtPath event) (Replica.evtType event) (V.DOMEvent $ Replica.evtEvent event)
    , Just <$> MVar.takeMVar stRef
    )
