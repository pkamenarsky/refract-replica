{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.Trans.State (get, modify)
import qualified Control.Monad.Trans.State as ST
import Control.Monad.IO.Class (liftIO)

import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as H
import Data.Text (Text, pack)
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

import Prelude hiding (div, span)

stateL :: Lens' st a -> (a -> Component st) -> Component st
stateL l f = state $ \st -> f (view l st)

toLens :: b -> AffineTraversal' a b -> Lens' a b
toLens d t = lens (fromMaybe d . preview t) (\a b -> set t b a)

unsafeIx :: Int -> Lens' [a] a
unsafeIx x = toLens (error "unsafeIx") $ ix x

tuple :: Lens' st a -> Lens' st b -> Lens' st (a, b)
tuple x y = lens (\st -> (view x st, view y st)) (\st (a, b) -> set y b $ set x a st)

px :: Int -> Text
px x = pack (show x) <> "px"

-- Tree ------------------------------------------------------------------------

data NodeState = NodeState
  { _nodeOpen :: Bool
  , _nodeName :: Text
  , _nodeChildren :: [NodeState]
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

makeLenses ''NodeState

defaultNodeState :: NodeState
defaultNodeState = NodeState False "/root"
  [ NodeState False "/home"
      [ NodeState False "/phil" []
      , NodeState False "/satan" []
      ]
  , NodeState False "/etc" []
  ]

showTree :: Int -> Lens' st NodeState -> Component st
showTree level l = stateL l $ \NodeState {..} -> div [ style css ] $ mconcat
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
    css =
      [ ("paddingLeft", pack (show $ level * 12) <> "px")
      , ("fontFamily", "Helvetica")
      , ("fontSize", "14px")
      , ("lineHeight", "18px")
      ]

-- Window  ---------------------------------------------------------------------

data WindowState w = WindowState
  { _positionX :: Int
  , _positionY :: Int
  , _width :: Int
  , _height :: Int
  , _dragX :: Int
  , _dragY :: Int
  , _windowState :: w
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

makeLenses ''WindowState

defaultWindowState :: WindowState ()
defaultWindowState = WindowState
  { _positionX = 100
  , _positionY = 100
  , _width = 200
  , _height = 200
  , _dragX = 0
  , _dragY = 0
  , _windowState = ()
  }

window :: Component st -> DragHandler st -> Lens' st (WindowState a) -> Component st
window cmp startDrag l = stateL l $ \rs -> div
  [ css rs ]
  [ div
      [ header
      , onMouseDown $ \e -> startDrag e
          (\_ _ -> pure ())
          (\x y -> modify $ over l $ \rs' -> rs' { _dragX = x, _dragY = y })
          (modify $ over l $ \rs' -> rs' { _positionX = _positionX rs' + _dragX rs', _positionY = _positionY rs' + _dragY rs', _dragX = 0, _dragY = 0 })
      ] []
  , div [ content ] [ cmp ]
  ]
  where
    css WindowState {..} = style
      [ ("position", "absolute")
      , ("left", px (_positionX + _dragX))
      , ("top", px (_positionY + _dragY))
      , ("width", px _width)
      , ("height", px _height)
      , ("border", "1px solid #333")
      , ("borderRadius", "5px 5px 0px 0px")
      ]
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

data DroppedState = DroppedState
  { _dsX :: Int
  , _dsY :: Int
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

makeLenses ''DroppedState

data DraggableState = DraggableState
  { _ssDragged :: Bool
  , _ssDragX :: Int
  , _ssDragY :: Int
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

makeLenses ''DraggableState

defaultDraggableState :: DraggableState
defaultDraggableState = DraggableState
  { _ssDragged = False
  , _ssDragX = 0
  , _ssDragY = 0
  }

shareable
  :: DragHandler st
  -> Lens' st DraggableState
  -> Lens' st [DroppedState]
  -> Props st
shareable startDrag l lds = onMouseDown $ \e -> startDrag e
  (\_ _ -> modify $ over l $ \st' -> st' { _ssDragged = True } )
  (\x y -> modify $ over l $ \st' -> st' { _ssDragX = x, _ssDragY = y } )
  $ do
      st <- view l <$> get
      modify $ over lds $ \st' -> DroppedState { _dsX = 300 + _ssDragX st, _dsY = 300 + _ssDragY st }:st'
      modify $ over l $ \st' -> st' { _ssDragged = False, _ssDragX = 0, _ssDragY = 0 }

song :: DragHandler st -> Lens' st DraggableState -> Lens' st [DroppedState] -> Component st
song startDrag l lds = stateL l $ \st -> div []
  [ div [ css ]
      [ div
          [ style dragHandle
          , shareable startDrag l lds
          ] []
      ]
  , if _ssDragged st
      then div [ dragged st ] []
      else div [] []
  ]
  where
    css = style
      [ ("position", "absolute")
      , ("left", px 300)
      , ("top", px 300)
      , ("width", px 200)
      , ("height", px 50)
      , ("border", "1px solid #333")
      ]

    dragHandle = 
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
dropped l = stateL l $ \st ->div [ css st ] []
  where
    css DroppedState {..} = style
      [ ("position", "absolute")
      , ("left", px _dsX)
      , ("top", px _dsY)
      , ("width", px 50)
      , ("height", px 50)
      , ("backgroundColor", "#333")
      ]

--------------------------------------------------------------------------------

data SongState = SongState deriving (Show, Generic, A.FromJSON, A.ToJSON)
data PlaylistState = PlaylistState deriving (Show, Generic, A.FromJSON, A.ToJSON)

data InstanceType st where
  InstanceTree :: InstanceType NodeState
  InstanceSong :: InstanceType SongState
  InstancePlaylist :: InstanceType PlaylistState

instance A.ToJSON (InstanceType st) where
  toJSON InstanceTree = A.String "tree"
  toJSON InstanceSong = A.String "song"
  toJSON InstancePlaylist = A.String "playlist"

data PathSegment = Key Text | Index Int deriving (Show, Generic, A.FromJSON, A.ToJSON)
type Path = [PathSegment]

keyLens :: Text -> Lens' A.Value A.Value
keyLens = undefined

indexLens :: Int -> Lens' A.Value A.Value
indexLens = undefined

pathToLens :: A.FromJSON a => Path -> Lens' A.Value a
pathToLens path = lens (get path) undefined
  where
    pathLens (Key k:ps) = keyLens k % pathLens ps
    pathLens (Index i:ps) = indexLens i % pathLens ps

    get [] o
      | A.Success v <- A.fromJSON o = v
      | otherwise = error "pathToLens"
    get (Key k:ps) (A.Object o)
      | Just v <- H.lookup k o = get ps v
    get (Index i:ps) (A.Array a)
      | Just v <- a V.!? i = get ps v
    get _ _ = error "pathToLens"

data Instance = forall sti. (A.FromJSON sti, A.ToJSON sti) => Instance (InstanceType sti) Path

instance A.FromJSON Instance where
  parseJSON (A.Object o) = do
    A.String t <- o A..: "type"
    p <- o A..: "path"

    case t of
      "tree" -> pure $ Instance InstanceTree p
      "song" -> pure $ Instance InstanceSong p
      "playlist" -> pure $ Instance InstancePlaylist p

componentForInstance :: Instance -> Component A.Value
componentForInstance (Instance t@InstanceTree p) = showTree 0 (pathToLens p)

data WindowType st w where
  Folder :: WindowType st [Component st]

windowOnDrop :: WindowType st w -> WindowState w -> WindowState w
windowOnDrop Folder wst = wst

-- OS --------------------------------------------------------------------------

data State = State
  { _root :: NodeState
  , _windowStates :: [WindowState ()]
  , _draggableState :: DraggableState
  , _droppedState :: [DroppedState]
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

makeLenses ''State

defaultState = State
  { _root = defaultNodeState
  , _windowStates = [defaultWindowState, defaultWindowState]
  , _draggableState = defaultDraggableState
  , _droppedState = []
  }

main :: IO ()
main = do
  runDefault 3777 "Tree" storeState readStore (pure <$> newTChanIO) $ \ctx [ddChan] -> do
    pure $ let drag = startDrag ctx ddChan in state $ \st ->
      div [] $ mconcat
        [ -- [ text (pack $ show st) ]
          [ window (showTree 0 root) drag (windowStates % unsafeIx  i)
          | (i, _) <- zip [0..] (_windowStates st)
          ]
        , [ song drag draggableState droppedState ]
        , [ dropped (droppedState % unsafeIx  i)
          | (i, _) <- zip [0..] (_droppedState st)
          ]
        ]
  where
    storeState st = Store.writeStore (Store.Store 0) $ A.encode st

    readStore = fromMaybe defaultState <$> do
      store <- Store.lookupStore 0

      case store of
        Nothing -> do
          storeState defaultState
          pure $ Just defaultState
        Just store -> A.decode <$> Store.readStore (Store.Store 0)
