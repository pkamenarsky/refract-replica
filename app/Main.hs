{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Trans.State (get, modify)
import qualified Control.Monad.Trans.State as ST
import Control.Monad.IO.Class (liftIO)

import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Optics as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as H
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Tuple.Optics
import Data.Maybe (fromMaybe)
import Data.Maybe.Optics
import qualified Data.Vector as V

import qualified Foreign.Store as Store

import Optics.Getter
import Optics.At
import Optics.AffineFold
import Optics.AffineTraversal
import Optics.Iso
import Optics.Lens
import Optics.Optic
import Optics.Setter
import Optics.TH

import Refract.DOM.Events
import Refract.DOM.Props
import Refract.DOM
import Refract

import GHC.Generics (Generic)

import qualified Network.Wai.Handler.Replica as R

import Types

import Prelude hiding (div, span)

stateL :: Lens' st a -> (a -> Component st) -> Component st
stateL l f = state $ \st -> f (view l st)

zoom :: Lens' st a -> Component a -> Component st
zoom l cmp = Component $ \path setState st -> runComponent cmp path (\a -> setState (set l a st)) (view l st)

px :: Int -> Text
px x = pack (show x) <> "px"

-- Tree ------------------------------------------------------------------------

showTree :: Int -> Lens' st NodeState -> Component st
showTree level l = stateL l $ \NodeState {..} -> div [ frame ] $ mconcat
  [ [ span
        [ onClick $ \_ -> modify $ over (l % nodeOpen) not ]
        [ text $ (if _nodeOpen then "-" else "+") <> _nodeName ]
    ]

  , if _nodeOpen
      then
        [ showTree (level + 1) (l % nodeChildren % unsafeIx i)
        | (i, _) <- zip [0..] _nodeChildren
        ]  
      else []
  ]
  where
    frame = style
      [ ("paddingLeft", pack (show $ level * 12) <> "px")
      , ("fontFamily", "Helvetica")
      , ("fontSize", "14px")
      , ("lineHeight", "18px")
      ]

-- Window  ---------------------------------------------------------------------

window :: Component st -> StartDrag st -> Lens' st WindowState -> Component st
window cmp startDrag l = stateL l $ \ws -> div
  [ frame ws ]
  [ div
      [ header
      , onMouseDown $ \e -> startDrag e dragStarted dragDragging dragDropped
      ] []
  , div [ content ] [ cmp ]
  ]
  where
    dragStarted _ _ = modify $ set (l % wndDragOffset % _Just) origin
    dragDragging x y = modify $ set (l % wndDragOffset % _Just) (Point x y)
    dragDropped = modify $ over l $ \st@(WindowState { _wndRect = Rect x y w h, _wndDragOffset = offset}) -> st
      { _wndRect = case offset of
          Just (Point ox oy) -> Rect (x + ox) (y + oy) w h
          Nothing -> _wndRect st
      , _wndDragOffset = Nothing
      }

    frame WindowState {..} = style
      [ ("position", "absolute")
      , ("left", px (x + ox))
      , ("top", px (y + oy))
      , ("width", px w)
      , ("height", px y)
      , ("border", "1px solid #333")
      , ("borderRadius", "5px 5px 0px 0px")
      ]
      where
        Rect x y w h = _wndRect
        Point ox oy = fromMaybe origin _wndDragOffset

    header = style
      [ ("position", "absolute")
      , ("left", px 0)
      , ("top", px 0)
      , ("width", "100%")
      , ("height", px 24)
      , ("backgroundColor", "#333")
      , ("borderRadius", "2px 2px 0px 0px")
      ]
    content = style
      [ ("position", "absolute")
      , ("left", px 0)
      , ("top", px 24)
      , ("width", "100%")
      , ("bottom", px 0)
      , ("backgroundColor", "#fff")
      , ("userSelect", "none")
      , ("overflow", "auto")
      ]

-- Song ------------------------------------------------------------------------

shareable
  :: StartDrag st
  -> IO Bounds
  -> Instance
  -> Lens' st (Maybe InstanceState)
  -> Props st
shareable startDrag getParentBounds inst l = onMouseDown $ \e -> startDrag e
  dragStarted
  dragDragging
  dragDropped
  where
    dragStarted x y = do
      (x, y, w, h) <- liftIO getParentBounds
      modify $ set l $ Just $ InstanceState
        { _instWindowState = WindowState
            { _wndRect = Rect (round x) (round y) (round w) (round h)
            , _wndDragOffset = Just origin
            }
        , _instInstance = inst
        }
    dragDragging x y = modify $ set (l % _Just % instWindowState % wndDragOffset % _Just) (Point x y)
    dragDropped = do
      -- mSt <- view l <$> get
      -- whenJust mSt $ \st -> modify $ over lds $ \st' -> DroppedState { _dsX = 300 + _ssDragX st, _dsY = 300 + _ssDragY st }:st'
      modify $ set l Nothing

song :: GetBounds -> StartDrag st -> Lens' st (Maybe InstanceState) -> Component st
song getBounds startDrag l = stateL l $ \st -> div []
  [ domPath $ \path -> div [ frame ]
      [ div
          [ dragHandle
          , shareable startDrag (getBounds path) InstanceRect l
          ] []
      ]
  ]
  where
    frame = style
      [ ("position", "absolute")
      , ("left", px 300)
      , ("top", px 300)
      , ("width", px 200)
      , ("height", px 50)
      , ("border", "1px solid #333")
      ]

    dragHandle = style
      [ ("position", "absolute")
      , ("top", px 8)
      , ("right", px 8)
      , ("width", px 8)
      , ("height", px 8)
      , ("backgroundColor", "#333")
      ]

    dragged DraggableState {..} = style
      [ ("position", "absolute")
      , ("left", px (300 + _ssDragX))
      , ("top", px (300 + _ssDragY))
      , ("width", px 200)
      , ("height", px 50)
      , ("backgroundColor", "#333")
      ]

dropped :: Lens' st DroppedState -> Component st
dropped l = stateL l $ \st ->div [ frame st ] []
  where
    frame DroppedState {..} = style
      [ ("position", "absolute")
      , ("left", px _dsX)
      , ("top", px _dsY)
      , ("width", px 50)
      , ("height", px 50)
      , ("backgroundColor", "#333")
      ]

--------------------------------------------------------------------------------

safeguard :: Path -> AffineTraversal' st a -> (Lens' st a -> Component st) -> Component st
safeguard p l f = state $ \st -> case preview l st of
  Nothing -> div [ frame ] []
  Just _ -> f (toLens (error "defaultComponent") l)
  where
    frame = style
      [ ("position", "absolute")
      , ("left", px 0) -- TODO
      , ("top", px 0)
      , ("width", px 50)
      , ("height", px 50)
      , ("backgroundColor", "#f00")
      ]

componentForInstance :: StartDrag st -> Lens' st InstanceState -> Lens' st A.Value -> Component st
componentForInstance startDrag lis lv = stateL lis $ \st -> case _instInstance st of
  InstanceRect -> div [ fill ] []
  InstanceTree p-> window (safeguard p (lv % pathToLens p) (showTree 0)) startDrag (lis % instWindowState)
  where
    fill = style
      [ ("position", "absolute")
      , ("left", px 0)
      , ("top", px 0)
      , ("right", px 0)
      , ("bottom", px 0)
      , ("backgroundColor", "#f00")
      ]

-- dropOnInstance :: DraggableState -> Instance -> A.Value -> A.Value
-- dropOnInstance st (InstanceTree p) = over (pathToLens p) dropOnTree
--   where
--     dropOnTree :: NodeState -> NodeState
--     dropOnTree = undefined
-- dropOnInstance st (InstanceSong p) = over (pathToLens p) dropOnSong
--   where
--     dropOnSong :: SongState -> SongState
--     dropOnSong = undefined

-- OS --------------------------------------------------------------------------

main :: IO ()
main = do
  runDefault 3777 "Tree" storeState readStore (pure <$> newTChanIO) $ \ctx [ddChan] ->
    let drag = startDrag ctx ddChan
        bounds = getBounds ctx
      in state $ \st -> div [ frame ] $ mconcat
        [ [ text (pack $ show st) ]
        , [ div [ onClick $ \_ -> liftIO $ R.call ctx () "document.body.requestFullscreen()" ] [ text "Enter fullscreen" ] ]
        , [ window (showTree 0 nodeState) drag (windowStates % unsafeIx  i)
          | (i, _) <- zip [0..] (_windowStates st)
          ]
        , [ song bounds drag draggableInstance ]
        , [ dropped (droppedState % unsafeIx  i)
          | (i, _) <- zip [0..] (_droppedState st)
          ]
        -- , [ zoom global $ componentForInstance inst
        --   | inst <- _instances st
        --   ]
        ]
  where
    frame = style
      [ ("backgroundColor", "#bbb")
      , ("width", "100%")
      , ("height", "1000px")
      , ("margin", px 0)
      , ("padding", px 0)
      ]

    storeState st = Store.writeStore (Store.Store 0) $ A.encode st

    readStore = fromMaybe defaultState <$> do
      store <- Store.lookupStore 0

      case store of
        Nothing -> do
          storeState defaultState
          pure $ Just defaultState
        Just store -> A.decode <$> Store.readStore (Store.Store 0)
