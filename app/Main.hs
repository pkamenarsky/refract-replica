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

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Optics as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Tuple.Optics
import Data.Maybe (fromMaybe, isJust)
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
import Refract.DOM.Props hiding (width, height)
import Refract.DOM
import Refract

import GHC.Generics (Generic)

import Replica.VDOM.Types (EventOptions (EventOptions))
import qualified Network.Wai.Handler.Replica as R

import Layout
import Types

import Prelude hiding (div, span)

stateL :: Lens' st a -> (a -> Component st) -> Component st
stateL l f = state $ \st -> f (view l st)

zoom :: Lens' st a -> Component a -> Component st
zoom l cmp = Component $ \path setState st -> runComponent cmp path (\a -> setState (set l a st)) (view l st)

onTrackedDragEnd :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onTrackedDragEnd = onMouseUp

oneOrWarning :: Path -> AffineTraversal' st a -> (Lens' st a -> Component st) -> Component st
oneOrWarning p l f = state $ \st -> case preview l st of
  Nothing -> div [ frame ] []
  Just _ -> f (toLens (error "defaultComponent") l)
  where
    frame = style $ mconcat
      [ posAbsFill
      , [ backgroundColor "#f00" ]
      ]

oneOrEmpty :: AffineTraversal' st a -> (Lens' st a -> Component st) -> Component st
oneOrEmpty l f = state $ \st -> case preview l st of
  Nothing -> empty
  Just _ -> f (toLens (error "defaultComponent") l)

-- Shareable -------------------------------------------------------------------

shareable
  :: StartDrag st
  -> IO Bounds
  -> Instance
  -> Lens' st (Maybe InstanceState)
  -> Props st
shareable startDrag getParentBounds inst l = onMouseDown $ \e -> startDrag e dragStarted dragDragging dragDropped
  where
    dragStarted _ _ = do
      (x, y, w, h) <- liftIO getParentBounds

      modify $ set l $ Just $ InstanceState
        { _instName = "DRAGGED"
        , _instWindowState = WindowState
            { _wndRect = Rect (round x) (round y) (round w) (round h)
            , _wndDragOffset = Just origin
            , _wndTitleBar = False
            }
        , _instInstance = inst
        }
    dragDragging x y = modify $ set (l % _Just % instWindowState % wndDragOffset % _Just) (Point x y)
    dragDropped = modify $ set l Nothing

-- Window  ---------------------------------------------------------------------

layout :: Lens' st (Maybe InstanceState) -> Component st
layout l = stateL l $ \draggedInst -> div [ frame draggedInst ] (dropTargets 0)
  where
    frame draggedInst = style $ mconcat
      [ [ posAbsolute, left (px 0), top (px 0), right (px 0), bottom (px 0) ]
      , case draggedInst of
          Just _ -> []
          Nothing -> [ ("pointer-events", "none") ]
      , [ -- ("background-color", "#000")
        -- , ("opacity", "0.5")
        ]
      ]

    dropTargets level
      | level >= 2 = []
      -- | level == 2 = [ div [ mid ] [] ]
      | otherwise = 
          [ div [ hl ] [ div [ vt ] (dropTargets (level + 1)), div [ vb ] (dropTargets (level + 1)) ]
          , div [ hr ] [ div [ vt ] (dropTargets (level + 1)), div [ vb ] (dropTargets (level + 1)) ]
          , div [ mid, onMouseUp $ \_ -> modify $ set (l % _Just % instWindowState % wndRect) (Rect 0 0 100 100) ] []
          , div [ rl ] []
          , div [ ll ] []
          , div [ tl ] []
          , div [ bl ] []
          ]

    mid = style
      [ posAbsolute, left (pct 50), top (pct 50), width (px 48), height (px 48)
      , ("margin-left", "-24px")
      , ("margin-top", "-24px")
      , ("border-radius", "5px")
      , backgroundColor "#000"
      , ("opacity", "0.5")
      ]
    tl = style
      [ posAbsolute, left (pct 50), top (pct 50), width (px 48), height (px 24)
      , ("margin-left", "-24px")
      , ("margin-top", "-60px")
      , ("border-radius", "5px")
      , backgroundColor "#000"
      , ("opacity", "0.5")
      ]
    bl = style
      [ posAbsolute, left (pct 50), top (pct 50), width (px 48), height (px 24)
      , ("margin-left", "-24px")
      , ("margin-top", "36px")
      , ("border-radius", "5px")
      , backgroundColor "#000"
      , ("opacity", "0.5")
      ]
    rl = style
      [ posAbsolute, left (pct 50), top (pct 50), width (px 24), height (px 48)
      , ("margin-left", "36px")
      , ("margin-top", "-24px")
      , ("border-radius", "5px")
      , backgroundColor "#000"
      , ("opacity", "0.5")
      ]
    ll = style
      [ posAbsolute, left (pct 50), top (pct 50), width (px 24), height (px 48)
      , ("margin-left", "-60px")
      , ("margin-top", "-24px")
      , ("border-radius", "5px")
      , backgroundColor "#000"
      , ("opacity", "0.5")
      ]
    hl = style
      [ posAbsolute, left (px 0), top (px 0), bottom (px 0), right (pct 50)
      -- , borderRight (solidBorder "#777" 1)
      ]
    hr = style
      [ posAbsolute, left (pct 50), top (px 0), bottom (px 0), right (px 0)
      ]
    vt = style
      [ posAbsolute, left (px 0), top (px 0), right (px 0), bottom (pct 50)
      -- , borderBottom (solidBorder "#777" 1)
      ]
    vb = style
      [ posAbsolute, left (px 0), top (pct 50), right (px 0), bottom (px 0)
      ]

window :: Text -> Component st -> StartDrag st -> Lens' st (Maybe Text) -> Lens' st WindowState -> Component st
window instName cmp startDrag ldw l = stateL l $ \ws -> div
  [ frame ws ]
  [ div
      [ header (_wndTitleBar ws)
      , onMouseDown $ \e -> startDrag e dragStarted dragDragging dragDropped
      ] []
  , div [ content (_wndTitleBar ws) ] [ cmp ]
  ]
  where
    dragStarted _ _ = do
      modify $ set (l % wndDragOffset) (Just origin)
      modify $ set ldw (Just instName)
    dragDragging x y = modify $ set (l % wndDragOffset % _Just) (Point x y)
    dragDropped = do
      modify $ over l $ \st@(WindowState { _wndRect = Rect x y w h, _wndDragOffset = offset}) -> st
        { _wndRect = case offset of
            Just (Point ox oy) -> Rect (x + ox) (y + oy) w h
            Nothing -> _wndRect st
        , _wndDragOffset = Nothing
        }
      modify $ set ldw Nothing

    frame WindowState {..} = style
      [ ("position", "absolute")
      , ("left", px (x + ox))
      , ("top", px (y + oy))
      , ("width", px w)
      , ("height", px h)
      , ("border", "1px solid #333")
      , ("border-radius", if _wndTitleBar then "5px 5px 0px 0px" else "")
      , ("pointer-events", case _wndDragOffset of
            Just _ -> "none"
            Nothing -> "auto"
        )
      , ("user-select", "none")
      ]
      where
        Rect x y w h = _wndRect
        Point ox oy = fromMaybe origin _wndDragOffset

    header bar = style
      [ ("position", "absolute")
      , ("left", px 0)
      , ("top", px 0)
      , ("width", "100%")
      , ("height", px (if bar then 24 else 0))
      , ("background-color", "#333")
      , ("border-radius", "2px 2px 0px 0px")
      , ("user-select", "none")
      ]
    content bar = style
      [ ("position", "absolute")
      , ("left", px 0)
      , ("top", px (if bar then 24 else 0))
      , ("width", "100%")
      , ("bottom", px 0)
      , ("background-color", "#fff")
      , ("user-select", "none")
      , ("overflow", "auto")
      ]

-- Instances -------------------------------------------------------------------

windowForInstance
  :: GetBounds
  -> StartDrag st
  -> Lens' st (Maybe Text)
  -> Lens' st (Maybe InstanceState)
  -> Lens' st A.Value
  -> Lens' st InstanceState
  -> Component st
windowForInstance getBounds startDrag ldw ld lv lis = stateL ld $ \draggedInst -> stateL lis $ \st -> wrap (_instName st) $ case _instInstance st of
  InstanceRect -> div [ fill ] []
  InstanceTree st -> oneOrWarning st (lv % pathToLens st) (showTree draggedInst)
  InstanceSong st -> oneOrWarning st (lv % pathToLens st) (song getBounds startDrag ld)
  InstanceInspector st v -> oneOrWarning st (lv % pathToLens st) (inspector draggedInst (lv % pathToLens v))
  where
    wrap name cmp = window name cmp startDrag ldw (lis % instWindowState)

    fill = style
      [ ("position", "absolute")
      , ("left", px 0)
      , ("top", px 0)
      , ("right", px 0)
      , ("bottom", px 0)
      , ("background-color", "#f00")
      ]

-- Tree ------------------------------------------------------------------------

showTree :: Maybe InstanceState -> Lens' st NodeState -> Component st
showTree draggedInst l = div
  [ onTrackedDragEnd $ \e -> case draggedInst of
      Just inst -> do
        liftIO $ print e
        modify $ set l $ NodeState "RECT" False (NodeArray True [])
      Nothing -> pure ()
  ]
  [ go 0 l ]
  where
    go :: Int -> Lens' st NodeState -> Component st
    go level l = stateL l $ \NodeState {..} -> div [ frame ] $ mconcat
      [ [ span
            [ style [ ("border-top", if _nodeMouseOver && isJust draggedInst then "2px solid #333" else "") ]
            , onClick $ \_ -> modify $ over (l % nodeChildren % _NodeArray % _1) not
            , onMouseEnter $ \_ -> modify $ set (l % nodeMouseOver) True
            , onMouseLeave $ \_ -> modify $ set (l % nodeMouseOver) False
            ]
            [ text $ case _nodeChildren of
                NodeArray True _ -> "- " <> _nodeName
                NodeArray False _ -> "+ " <> _nodeName
                NodeString v -> _nodeName <> ": " <> v
                NodeNumber v -> _nodeName <> ": " <> pack (show v)
                NodeBool v -> _nodeName <> ": " <> if v then "true" else "false"
            ]
        ]
    
      , case _nodeChildren of
          NodeArray True children -> 
            [ go (level + 1) (toLens (error "showTree") $ l % nodeChildren % _NodeArray % _2 % ix i)
            | (i, _) <- zip [0..] children
            ]  
          _ -> []
      ]
      where
        frame = style
          -- [ ("paddingLeft", pack (show $ level * 12) <> "px")
          [ ("padding-left", px 12)
          , ("font-family", "Helvetica")
          , ("font-size", "14px")
          , ("line-height", "20px")
          ]

inspector :: Maybe InstanceState -> AffineTraversal' st A.Value -> Lens' st InspectorState -> Component st
inspector draggedInst lv l = div
  [ onTrackedDragEnd $ \e -> case draggedInst of
      Just inst -> do
        modify $ set lv A.Null
        modify $ set l defaultInspectorState
      Nothing -> pure ()
  ]
  [ go "root" "root" lv ]
  where
    go path nodeName lv = state $ \st -> let value = preview lv st in stateL l $ \InspectorState {..} -> div [ frame ] $ mconcat
      [ [ span
            [ style [ ("border-top", if Just path == _inspMouseOverNode && isJust draggedInst then "2px solid #333" else "") ]
            , onClick $ \_ -> modify $ over (l % inspOpenNodes) (toggle path)
            , onMouseEnter $ \_ -> modify $ set (l % inspMouseOverNode) (Just path)
            , onMouseLeave $ \_ -> modify $ set (l % inspMouseOverNode) Nothing
            ]
            $ case value of
                Just A.Null -> [ text $ nodeName <> ": <null>" ]
                Just (A.String v) -> [ text $ nodeName <> ": " <> v ]
                Just (A.Bool v) -> [ text $ nodeName <> ": " <> if v then "true" else "false" ]
                Just (A.Number v) -> [ text $ nodeName <> ": " <> pack (show v) ]
                Just (A.Object o) -> [ text $ (if isOpen _inspOpenNodes then "- " else "+ ") <> nodeName ]
                Just (A.Array o) -> [ text $ (if isOpen _inspOpenNodes then "- " else "+ ") <> nodeName ]
                _ -> []
        ]
      , if isOpen _inspOpenNodes
          then case value of
            Just (A.Object o) ->
              [ go (path <> "." <> K.toText k) (K.toText k) (lv % A.key (K.toText k))
              | (k, v) <- KM.toList o
              ]
            Just (A.Array o) ->
              [ go (path <> "[" <> pack (show k) <> "]") (pack $ show k) (lv % A.nth k)
              | (k, v) <- zip [0..] (V.toList o)
              ]
            _ -> []
          else []
      ]
      where
        isOpen hs = HS.member path hs

        toggle v hs
          | HS.member v hs = HS.delete v hs
          | otherwise = HS.insert v hs

        frame = style
          [ ("padding-left", px 12)
          , ("font-family", "Helvetica")
          , ("font-size", "14px")
          , ("line-height", "20px")
          ]

-- Song ------------------------------------------------------------------------

song
  :: GetBounds
  -> StartDrag st
  -> Lens' st (Maybe InstanceState)
  -> Lens' st SongState
  -> Component st
song getBounds startDrag ld l = div []
  [ domPath $ \path -> div [ frame ]
      [ div
          [ dragHandle
          , shareable startDrag (getBounds path) InstanceRect ld
          ] []
      ]
  ]
  where
    frame = style
      [ ("position", "absolute")
      , ("left", px 0)
      , ("top", px 0)
      , ("right", px 0)
      , ("bottom", px 0)
      ]

    dragHandle = style
      [ ("position", "absolute")
      , ("top", px 8)
      , ("right", px 8)
      , ("width", px 8)
      , ("height", px 8)
      , ("background-color", "#333")
      ]

-- OS --------------------------------------------------------------------------

main :: IO ()
main = do
  runDefault 3777 "Tree" storeState readStore channels $ \ctx [ddChan, keyChan] ->
    let drag = startDrag ctx ddChan
        bounds = getBounds ctx
      in state $ \st -> div [ frame ] $ mconcat
        [ [ div [ style [ ("user-select", "none") ] ] [ text (pack $ show st) ] ]
        -- , [ div [ onClick $ \_ -> liftIO $ R.call ctx () "document.body.requestFullscreen()" ] [ text "Enter fullscreen" ] ]

        -- Instances
        , [ windowForInstance bounds drag draggedWindow draggedInstance global (toLens (error "at") $ instances % at i % _Just)
          | (i, _) <- H.toList (_instances st)
          ]

        -- Dragged instance
        , [ oneOrEmpty (draggedInstance % _Just) $ windowForInstance bounds drag draggedWindow draggedInstance global ]

        , if _ctrlPressed st
            then [ layout (instances % at (fromMaybe "NONE" $ _draggedWindow st)) ]
            else []
        ]
  where
    channels ctx = do
      ddChan <- newTChanIO
      keyChan <- newTChanIO
      keyEvents ctx (keyDown keyChan) (keyUp keyChan)
      pure [ddChan, keyChan]
      where
        setCtrl v' v e
          | kbdKey e == "Alt" = v
          | otherwise = v'

        keyDown keyChan e = atomically $ writeTChan keyChan (\st -> pure $ st { _ctrlPressed = setCtrl (_ctrlPressed st) True e })
        keyUp keyChan e = atomically $ writeTChan keyChan (\st -> pure $ st { _ctrlPressed = setCtrl (_ctrlPressed st) False e })

    frame = style []
      -- [ ("backgroundColor", "#bbb")
      -- , ("width", "100%")
      -- , ("height", "1000px")
      -- , ("margin", px 0)
      -- , ("padding", px 0)
      -- ]

    storeState st = Store.writeStore (Store.Store 0) $ A.encode st

    readStore = fromMaybe defaultState <$> do
      store <- Store.lookupStore 0

      case store of
        Nothing -> do
          storeState defaultState
          pure $ Just defaultState
        Just store -> A.decode <$> Store.readStore (Store.Store 0)

--------------------------------------------------------------------------------

main2 :: IO ()
main2 = do
  runDefault 3777 "Tree" (\_ -> pure ()) (pure Nothing) (\_ -> pure <$> newTChanIO) $ \ctx _ -> state $ \st ->
    div (props st)
      [ div
          [ innerFrame
          , onClick $ \_ -> liftIO $ print "inner click"
          ]
          []
      , text $ pack (show st)
      ]
  where
    props dragged = mconcat
      [ [ frame ]
      -- , onClickWithOptions (EventOptions True False False) $ \_ -> liftIO $ print "click"
      , case dragged of
          Just _ ->
            [ onMouseMoveWithOptions (EventOptions True False False) $ \e -> ST.put $ Just (mouseClientX e, mouseClientY e)
            , onMouseUp $ \_ -> ST.put Nothing
            ]
          Nothing -> [ onMouseDown $ \e -> ST.put $ Just (mouseClientX e, mouseClientY e) ]
      ]
    frame = style
      [ posAbsolute, left (px 100), top (px 100), width (px 300), height (px 300)
      , ("background-color", "#777")
      ]

    innerFrame = style
      [ posAbsolute, left (px 100), top (px 100), width (px 100), height (px 100)
      , ("background-color", "#333")
      ]
