module Refract where

import qualified Control.Concurrent.MVar as MVar

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

run :: Int -> V.HTML -> ConnectionOptions -> Middleware -> st -> (R.Context -> Component st) -> IO ()
run port index connectionOptions middleware st component
  = W.run port
  $ R.app index connectionOptions middleware st (step component)

runDefault :: Int -> T.Text -> st -> (R.Context -> Component st) -> IO ()
runDefault port title st component
  = W.run port
  $ R.app (V.defaultIndex title []) defaultConnectionOptions id st (step component)

step :: (Replica.Context -> Component st) -> Replica.Context -> st -> IO (V.HTML, Replica.Event -> Maybe (IO ()), IO (Maybe st))
step cmp ctx st = do
  stRef <- MVar.newEmptyMVar
  let html = Refract.runComponent (cmp ctx) (MVar.putMVar stRef) st
  pure
    ( html
    , \event -> V.fireEvent html (Replica.evtPath event) (Replica.evtType event) (V.DOMEvent $ Replica.evtEvent event)
    , Just <$> MVar.takeMVar stRef
    )
