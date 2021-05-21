{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Trans.State (get, modify)
import qualified Control.Monad.Trans.State as ST
import Control.Monad.IO.Class (liftIO)

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Optics as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
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
import Refract.DOM.Props hiding (min, max, id, width, height)
import Refract.DOM
import Refract

import Text.Pretty.Simple (pShowNoColor)

import GHC.Generics (Generic)

import Replica.VDOM.Types (EventOptions (EventOptions))
import qualified Network.Wai.Handler.Replica as R

import Layout
import Types

import Prelude hiding (div, span)
import qualified Prelude

div' :: Int -> Int -> Int
div' = Prelude.div

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

shareable :: Env st -> IO Bounds -> Instance -> Props st
shareable Env {..} getParentBounds inst = onMouseDown $ \e -> envStartDrag e dragStarted dragDragged dragDropped
  where
    dragStarted _ _ = do
      (x, y, w, h) <- liftIO getParentBounds

      modify $ set envLDraggedInst $ Just
       ( inst
       , DraggableState
           { _dsRect = Rect (round x) (round y) (round w) (round h)
           , _dsDragOffset = Just origin
           }
       )
    dragDragged _ x y = modify $ set (envLDraggedInst % _Just % _2 % dsDragOffset % _Just) (Point x y)
    dragDropped = modify $ set envLDraggedInst Nothing

-- Instances -------------------------------------------------------------------

componentForInstance
  :: Env st
  -> Instance
  -> Maybe (Lens' st Instance)
  -> Component st
componentForInstance env@(Env {..}) inst mlInst = stateL envLDraggedInst $ \mDraggedInst -> case inst of
  InstanceEmpty -> flip div [] $ mconcat
    [ -- [ fill ]
    case (mlInst, mDraggedInst) of
        (Just lInst, Just (draggedInst, _)) ->
          [ onMouseUp $ \_ -> do
              liftIO $ print "lalal"
              modify $ set lInst draggedInst
          , fill "#777"
          ]
        _ -> [ onMouseUp $ \_ -> liftIO $ print "lalal2"
             , fill "transparent"
             ]
    ]
  InstanceTree st -> oneOrWarning st (envLValue % pathToLens st) (showTree mDraggedInst)
  InstanceSong st -> oneOrWarning st (envLValue % pathToLens st) (song (not $ isJust mlInst) env)
  InstanceInspector st v -> oneOrWarning st (envLValue % pathToLens st) (inspector mDraggedInst (envLValue % pathToLens v))
  where
    fill c = style
      [ posAbsolute, left (px 0), top (px 0), right (px 0), bottom (px 0), backgroundColor c
      ]

-- Tree ------------------------------------------------------------------------

showTree :: Maybe DraggedInstance -> Lens' st NodeState -> Component st
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
          , ("user-select", "none")
          ]

inspector :: Maybe DraggedInstance -> AffineTraversal' st A.Value -> Lens' st InspectorState -> Component st
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
              [ go (path <> "." <> k) k (lv % A.key k)
              | (k, v) <- H.toList o
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

song :: Bool -> Env st -> Lens' st SongState -> Component st
song dragged env@(Env {..}) lSongState = div []
  [ domPath $ \path -> div [ frame ]
      [ flip div [] $ mconcat
          [ [ dragHandle ]
          , if dragged then [] else [ shareable env (envGetBounds path) (InstanceSong [Key "song"]) ]
          ]
      ]
  ]
  where
    frame = style $
      [ posAbsolute
      , left (px 0), top (px 0), right (px 0), bottom (px 0)
      -- , backgroundColor "#777"
      ] <> if dragged then [ ("pointer-events", "none") ] else []

    dragHandle = style $
      [ posAbsolute
      , top (px 8), left (px 8), width (px 8), height (px 8)
      , backgroundColor "#333"
      ] <> if dragged then [ ("pointer-events", "none") ] else []

-- Layout ----------------------------------------------------------------------

layout
  :: Env st
  -> Lens' st LayoutState
  -> (st -> st)
  -> Component st
layout env@(Env {..}) lLayoutState close = stateL lLayoutState $ \stLayoutState -> case stLayoutState of
  LayoutInstance inst -> domPath $ \path -> div [ fill 0 0 False ] $ mconcat
    [ [ div [ fill handleSize handleSize True ] [ componentForInstance env inst (Just $ unsafeToLens $ lLayoutState % _LayoutInstance) ] ]

    -- Split handles
    , [ div
          [ rightSplitHandle
          , onMouseDown $ \e -> envStartDrag e (dragStartedSplitX stLayoutState e (envGetBounds path)) (dragDraggedX lLayoutState) dragFinished
          ] []
      , div
          [ topSplitHandle
          , onMouseDown $ \e -> envStartDrag e (dragStartedSplitY stLayoutState e (envGetBounds path)) (dragDraggedY lLayoutState) dragFinished
          ] []
      ]

    -- Close button
    , [ div
          [ closeButton
          , onClick $ \_ -> modify close
          ]
          []
      ]
    ]
  LayoutHSplit x leftLayout rightLayout -> domPath $ \path -> div [ fill 0 0 False ]
    [ div [ hsplitLeft x ] [ layout env (unsafeToLens $ lLayoutState % _LayoutHSplit % _2) (over lLayoutState (const rightLayout)) ]
    , div [ hsplitRight x ] [ layout env (unsafeToLens $ lLayoutState % _LayoutHSplit % _3) (over lLayoutState (const leftLayout)) ]
    , div [ dragBarH x ] []
    , div
        [ dragBarHDraggable x
        , onMouseDown $ \e -> envStartDrag e (dragStartedX e (envGetBounds path)) (dragDraggedX lLayoutState) dragFinished
        ]
        []
    , div
        [ dragBarHSplitHandle x
        , onMouseDown $ \e -> envStartDrag e (dragStartedSplitY stLayoutState e (envGetBounds path)) (dragDraggedY lLayoutState) dragFinished
        ]
        []
    ]
  LayoutVSplit y topLayout bottomLayout -> domPath $ \path -> div [ fill 0 0 False ]
    [ div [ vsplitTop y ] [ layout env (unsafeToLens $ lLayoutState % _LayoutVSplit % _2) (over lLayoutState (const bottomLayout)) ]
    , div [ vsplitBottom y ] [ layout env (unsafeToLens $ lLayoutState % _LayoutVSplit % _3) (over lLayoutState (const topLayout)) ]
    , div [ dragBarV y ] []
    , div
        [ dragBarVDraggable y
        , onMouseDown $ \e -> envStartDrag e (dragStartedY e (envGetBounds path)) (dragDraggedY lLayoutState) dragFinished
        ]
        []
    , div
        [ dragBarVSplitHandle y
        , onMouseDown $ \e -> envStartDrag e (dragStartedSplitX stLayoutState e (envGetBounds path)) (dragDraggedX lLayoutState) dragFinished
        ]
        []
    ]
  where
    fi = fromIntegral

    dragStartedSplitX st e getBounds _ _ = do
      bounds <- liftIO getBounds
      modify $ set lLayoutState $ LayoutHSplit 90 st defaultLayoutState
      pure (e, bounds)
    dragStartedX e getBounds _ _ = do
      bounds <- liftIO getBounds
      pure (e, bounds)
    dragDraggedX l (e, (bx, _, bw, _)) x _ = do
      let xpct = ((fi (mouseClientX e + x) - bx) * 100.0 / bw)
      modify $ set (l % _LayoutHSplit % _1) (max 10 (min 90 xpct))
    dragFinished = pure ()

    dragStartedSplitY st e getBounds _ _ = do
      bounds <- liftIO getBounds
      modify $ set lLayoutState $ LayoutVSplit 10 defaultLayoutState st
      pure (e, bounds)
    dragStartedY e getBounds _ _ = do
      bounds <- liftIO getBounds
      pure (e, bounds)
    dragDraggedY l (e, (_, by, _, bh)) _ y = do
      let ypct = ((fi (mouseClientY e + y) - by) * 100.0 / bh)
      modify $ set (l % _LayoutVSplit % _1) (max 10 (min 90 ypct))

    transparent = "transparent"

    handleSize = 10
    handleColor = "#aaa"

    fill x y overflow = style
      [ posAbsolute, left (px 0), top (px y), right (px x), bottom (px 0)
      , ("overflow", if overflow then "auto" else "hidden")
      ]

    hsplitLeft x = style [ posAbsolute, left (px 0), top (px 0), width (pct x), bottom (px 0) ]
    hsplitRight x = style [ posAbsolute, left (pct x), top (px 0), right (px 0), bottom (px 0) ]
    vsplitTop y = style [ posAbsolute, left (px 0), top (px 0), right (px 0), height (pct y) ]
    vsplitBottom y = style [ posAbsolute, left (px 0), top (pct y), right (px 0), bottom (px 0) ]

    border which x color = ("border-" <> which, x <> " solid " <> color)

    dragBarHSplitHandle x = style
      [ posAbsolute, top (px 0), left (pct x), width (px 0), height (px handleSize)
      , marginLeft (px (-handleSize))
      , border "top" (px handleSize) handleColor
      , border "left" (px handleSize) transparent
      , border "right" (px handleSize) transparent
      ]

    dragBarHDraggable x = style
      [ posAbsolute, top (px 0), left (pct x), bottom (px 0), width (px 6)
      , marginLeft (px (-3))
      ]

    dragBarH x = style
      [ posAbsolute, top (px 0), left (pct x), bottom (px 0), width (px 1)
      , backgroundColor handleColor
      ]

    dragBarVSplitHandle y = style
      [ posAbsolute, right (px 0), top (pct y), width (px handleSize), height (px 0) 
      , marginTop (px (-handleSize))
      , border "right" (px handleSize) handleColor
      , border "top" (px handleSize) transparent
      , border "bottom" (px handleSize) transparent
      ]

    dragBarVDraggable y = style
      [ posAbsolute, left (px 0), top (pct y), right (px 0), height (px 6)
      , marginTop (px (-3))
      ]

    dragBarV y = style
      [ posAbsolute, left (px 0), top (pct y), right (px 0), height (px 1)
      , backgroundColor handleColor
      ]

    rightSplitHandle = style
      [ posAbsolute, top (pct 50), right (px 0), width (px handleSize), height (px 0)
      , marginTop (px (-handleSize))
      , border "right" (px handleSize) handleColor
      , border "top" (px handleSize) transparent
      , border "bottom" (px handleSize) transparent
      ]

    topSplitHandle = style
      [ posAbsolute, top (px 0), left (pct 50), width (px 0), height (px handleSize)
      , marginLeft (px (-handleSize))
      , border "top" (px handleSize) handleColor
      , border "left" (px handleSize) transparent
      , border "right" (px handleSize) transparent
      ]

    closeButton = style
      [ posAbsolute, top (px handleSize), right (px handleSize), width (px handleSize), height (px handleSize)
      , backgroundColor handleColor
      , ("border-radius", px handleSize)
      ]

-- OS --------------------------------------------------------------------------

data Env st = Env
  { envGetBounds :: GetBounds
  , envStartDrag :: StartDrag st
  , envLValue :: Lens' st A.Value
  , envLDraggedInst :: Lens' st (Maybe (Instance, DraggableState))
  }

main :: IO ()
main = do
  runDefault 3777 "Tree" storeState readStore channels $ \ctx [ddChan, keyChan] ->
    let env = Env
          { envGetBounds = getBounds ctx
          , envStartDrag = startDrag ctx ddChan
          , envLValue = global
          , envLDraggedInst = draggedInstance
          }
      in state $ \st -> div [ frame ] $ mconcat
        [ []
        -- , [ div [ style [ ("user-select", "none") ] ] [ text (pack $ show st) ] ]
        -- , [ div [ onClick $ \_ -> liftIO $ R.call ctx () "document.body.requestFullscreen()" ] [ text "Enter fullscreen" ] ]

        , [ layout env layoutState id ]

        -- Dragged instance
        , [ oneOrEmpty (draggedInstance % _Just) $ flip stateL $ \(inst, DraggableState (Rect x y w h) offset)  -> div
              [ style [ posAbsolute, left (px (x + xo offset)), top (px (y + yo offset)), width (px w), height (px h), border "1px solid #777", ("pointer-events", "none") ]
              ]
              [ componentForInstance env inst Nothing ]
          ]
        ]
  where
    xo = maybe 0 _pointX
    yo = maybe 0 _pointY

    channels ctx = do
      ddChan <- newTChanIO
      keyChan <- newTChanIO
      -- keyEvents ctx (keyDown keyChan) (keyUp keyChan)
      pure [ddChan, keyChan]
      where
        setCtrl v' v e
          | kbdKey e == "Alt" = v
          | otherwise = v'

        -- keyDown keyChan e = atomically $ writeTChan keyChan (\st -> pure $ st { _ctrlPressed = setCtrl (_ctrlPressed st) True e })
        -- keyUp keyChan e = atomically $ writeTChan keyChan (\st -> pure $ st { _ctrlPressed = setCtrl (_ctrlPressed st) False e })

    frame = style [ ("overflow", "hidden") ]
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
          let st = defaultState
                { _layoutState = LayoutHSplit 20 (LayoutInstance (InstanceSong [Key "song"])) (LayoutInstance InstanceEmpty)
                }
          storeState st
          pure $ Just st
        Just store -> A.decode <$> Store.readStore (Store.Store 0)

--------------------------------------------------------------------------------

test :: IO ()
test = do
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
