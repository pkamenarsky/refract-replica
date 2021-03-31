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

shareable
  :: Env st
  -> IO Bounds
  -> Instance
  -> Props st
shareable Env {..} getParentBounds inst = onMouseDown $ \e -> envStartDrag e dragStarted dragDragged dragDropped
  where
    dragStarted _ _ = do
      (x, y, w, h) <- liftIO getParentBounds

      modify $ set envLDraggedInst $ Just $ InstanceState
        { _instName = "DRAGGED"
        , _instWindowState = WindowState
            { _wndRect = Rect (round x) (round y) (round w) (round h)
            , _wndDragOffset = Just origin
            , _wndTitleBar = False
            }
        , _instInstance = inst
        }
    dragDragged _ x y = modify $ set (envLDraggedInst % _Just % instWindowState % wndDragOffset % _Just) (Point x y)
    dragDropped = modify $ set envLDraggedInst Nothing

-- Window  ---------------------------------------------------------------------

window :: Text -> Component st -> StartDrag st -> Lens' st WindowState -> Component st
window instName cmp startDrag l = stateL l $ \ws -> div
  [ frame ws ]
  [ div
      [ header (_wndTitleBar ws)
      , onMouseDown $ \e -> startDrag e dragStarted dragDragged dragDropped
      ] []
  , div [ content (_wndTitleBar ws) ] [ cmp ]
  ]
  where
    dragStarted _ _ = modify $ set (l % wndDragOffset) (Just origin)
    dragDragged _ x y = modify $ set (l % wndDragOffset % _Just) (Point x y)
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

componentForInstance
  :: Env st
  -> Instance
  -> Component st
componentForInstance env@(Env {..}) inst = stateL envLDraggedInst $ \draggedInst -> case inst of
  InstanceRect -> div [ fill ] []
  InstanceTree st -> oneOrWarning st (envLValue % pathToLens st) (showTree draggedInst)
  InstanceSong st -> oneOrWarning st (envLValue % pathToLens st) (song env)
  InstanceInspector st v -> oneOrWarning st (envLValue % pathToLens st) (inspector draggedInst (envLValue % pathToLens v))
  where
    fill = style
      [ posAbsolute, left (px 0), top (px 0), right (px 0), bottom (px 0)
      , backgroundColor "#f00"
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
          , ("user-select", "none")
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

song :: Env st -> Lens' st SongState -> Component st
song env@(Env {..}) lSongState = div []
  [ domPath $ \path -> div [ frame ]
      [ div
          [ dragHandle
          , shareable env (envGetBounds path) InstanceRect
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

-- Layout ----------------------------------------------------------------------

layout
  :: Env st
  -> LayoutState
  -> Lens' st LayoutState
  -> Component st
layout env@(Env {..}) tempLs lLayoutState = stateL lLayoutState $ \layoutState -> case layoutState of
  LayoutInstance name inst -> domPath $ \path -> div [ fill 0 0 False ]
    [ div [ fill 0 0 True ] [ {- componentForInstance env inst -} text $ pack (show $ handlesForLayout tempLs) ]
    , div
        [ dragHandleRight
        , onMouseDown $ \e -> envStartDrag e (dragStartedSplitX e name inst (envGetBounds path)) dragDraggedX dragFinished
        ] []
    , div
        [ dragHandleTop
        , onMouseDown $ \e -> envStartDrag e (dragStartedSplitY e name inst (envGetBounds path)) dragDraggedY dragFinished
        ] []
    ]
  LayoutHSplit x left right -> domPath $ \path -> div [ fill 0 0 False ]
    [ div [ hsplitLeft x ]
      [ div [ fill barSize 0 False ]
          [ layout env tempLs (unsafeToLens $ lLayoutState % _LayoutHSplit % _2)
          , div
              [ closeButton
              , onClick $ \_ -> modify $ set lLayoutState right
              ] []
          ]
      , div
          [ dragBarRight
          , onMouseDown $ \e -> envStartDrag e (dragStartedX e (envGetBounds path)) dragDraggedX dragFinished
          ] []
      ]
    , div [ hsplitRight x ]
        [ layout env tempLs (unsafeToLens $ lLayoutState % _LayoutHSplit % _3)
        , div
            [ closeButton
            , onClick $ \_ -> modify $ set lLayoutState left
            ] []
        ]
    ]
  LayoutVSplit y top bottom -> domPath $ \path -> div [ fill 0 0 False ]
    [ div [ vsplitTop y ]
        [ layout env tempLs (unsafeToLens $ lLayoutState % _LayoutVSplit % _2)
        , div
            [ closeButton
            , onClick $ \_ -> modify $ set lLayoutState bottom
            ] []
        ]
    , div [ vsplitBottom y ]
        [ div [ fill 0 barSize False ]
            [ layout env tempLs (unsafeToLens $ lLayoutState % _LayoutVSplit % _3)
            , div
                [ closeButton
                , onClick $ \_ -> modify $ set lLayoutState top
                ] []
            ]
        , div
            [ dragBarTop
            , onMouseDown $ \e -> envStartDrag e (dragStartedY e (envGetBounds path)) dragDraggedY dragFinished
            ] []
        ]
    ]
  where
    fi = fromIntegral

    dragStartedSplitX e name inst getBounds _ _ = do
      bounds <- liftIO getBounds
      modify $ set lLayoutState $ LayoutHSplit 100 (LayoutInstance name inst) defaultLayoutState
      pure (e, bounds)
    dragStartedX e getBounds _ _ = do
      bounds <- liftIO getBounds
      pure (e, bounds)
    dragDraggedX (e, (bx, _, bw, _)) x _ = do
      modify $ set (lLayoutState % _LayoutHSplit % _1) ((fi (mouseClientX e) + fi x - bx) * 100.0 / bw)
    dragFinished = pure ()

    dragStartedSplitY e name inst getBounds _ _ = do
      bounds <- liftIO getBounds
      modify $ set lLayoutState $ LayoutVSplit 100 (LayoutInstance name inst) defaultLayoutState
      pure (e, bounds)
    dragStartedY e getBounds _ _ = do
      bounds <- liftIO getBounds
      pure (e, bounds)
    dragDraggedY (e, (_, by, _, bh)) _ y = do
      modify $ set (lLayoutState % _LayoutVSplit % _1) ((fi (mouseClientY e) + fi y - by) * 100.0 / bh)

    barSize = 12
    barColor = "#aaa"

    fill x y overflow = style
      [ posAbsolute, left (px 0), top (px y), right (px x), bottom (px 0)
      , ("overflow", if overflow then "auto" else "hidden")
      ]

    hsplitLeft x = style
      [ posAbsolute, left (px 0), top (px 0), width (pct $ round x), bottom (px 0)
      , borderLeft (solidBorder barColor 1)
      ]
    hsplitRight x = style [ posAbsolute, left (pct $ round x), top (px 0), right (px 0), bottom (px 0) ]
    vsplitTop y = style
      [ posAbsolute, left (px 0), top (px 0), right (px 0), height (pct $ round y)
      , borderBottom (solidBorder barColor 1)
      ]
    vsplitBottom y = style [ posAbsolute, left (px 0), top (pct $ round y), right (px 0), bottom (px 0) ]

    dragBarRight = style
      [ posAbsolute, top (px 0), right (px 0), width (px barSize), bottom (px 0)
      , backgroundColor barColor
      ]
    dragBarTop = style
      [ posAbsolute, top (px 0), left (px 0), height (px barSize), right (px 0)
      , backgroundColor barColor
      ]

    handleSize = 24

    dragHandleRight = style
      [ posAbsolute, top (pct 50), right (px 0), width (px handleSize), height (px handleSize)
      , marginTop (px (-handleSize `div'` 2))
      , backgroundColor barColor
      ]
    dragHandleTop = style
      [ posAbsolute, top (px 0), left (pct 50), width (px handleSize), height (px handleSize)
      , marginLeft (px (-handleSize `div'` 2))
      , backgroundColor barColor
      ]
    closeButton = style
      [ posAbsolute, top (px 0), right (px 0), width (px handleSize), height (px handleSize)
      , backgroundColor barColor
      ]

data Node = HNode [Double] Node Node | VNode [Double] Node Node | INode
  deriving Show

handlesForLayout :: LayoutState -> (Node, [(LayoutState, Double)], [(LayoutState, Double)])
handlesForLayout (LayoutInstance _ _) = (INode, [], [])
handlesForLayout (LayoutHSplit x left right) =
  (HNode (map snd rightHandles <> [x]) leftNode rightNode
  , [ (st, x * tx) | (st, tx) <- leftTops ] <> [ (st, (100.0 - x) * tx + x) | (st, tx) <- rightTops ]
  , rightRights
  )
  where
    (leftNode, leftTops, rightHandles) = handlesForLayout left
    (rightNode, rightTops, rightRights) = handlesForLayout right
handlesForLayout (LayoutVSplit y top bottom) =
  (VNode (map snd topHandles <> [y]) topNode bottomNode
  , [ (st, y * ty) | (st, ty) <- topRights ] <> [ (st, (100.0 - y) * ty + y) | (st, ty) <- bottomRights ]
  , topTops
  )
  where
    (topNode, topTops, topRights) = handlesForLayout top
    (bottomNode, topHandles, bottomRights) = handlesForLayout bottom

-- OS --------------------------------------------------------------------------

data Env st = Env
  { envGetBounds :: GetBounds
  , envStartDrag :: StartDrag st
  , envLValue :: Lens' st A.Value
  , envLDraggedInst :: Lens' st (Maybe InstanceState)
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

        -- Dragged instance
        -- , [ oneOrEmpty (draggedInstance % _Just) $ windowForInstance undefined undefined draggedInstance global ]
        , [ layout env (_layoutState st) layoutState ]
        ]
  where
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
