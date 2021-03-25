{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import qualified Data.Aeson as A
import qualified Data.Aeson.Optics as A
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)

import Optics.Getter
import Optics.AffineFold
import Optics.AffineTraversal
import Optics.At
import Optics.Lens
import Optics.Optic
import Optics.Setter
import Optics.TH

import GHC.Generics (Generic)

-- Utils -----------------------------------------------------------------------

toLens :: b -> AffineTraversal' a b -> Lens' a b
toLens d t = lens (fromMaybe d . preview t) (\a b -> set t b a)

unsafeIx :: Int -> Lens' [a] a
unsafeIx x = toLens (error "unsafeIx") $ ix x

tuple :: Lens' st a -> Lens' st b -> Lens' st (a, b)
tuple x y = lens (\st -> (view x st, view y st)) (\st (a, b) -> set y b $ set x a st)

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = pure ()
whenJust (Just a) f = f a

-- Types -----------------------------------------------------------------------

data Rect = Rect Int Int Int Int
  deriving (Show, Generic, A.ToJSON, A.FromJSON)

data Point = Point { _pointX :: Int, _pointY :: Int }
  deriving (Show, Generic, A.ToJSON, A.FromJSON)

origin :: Point
origin = Point 0 0

data NodeState = NodeState
  { _nodeOpen :: Bool
  , _nodeName :: Text
  , _nodeChildren :: [NodeState]
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

defaultNodeState :: NodeState
defaultNodeState = NodeState False "/root"
  [ NodeState False "/home"
      [ NodeState False "/phil" []
      , NodeState False "/satan" []
      ]
  , NodeState False "/etc" []
  ]

data WindowState = WindowState
  { _wndRect :: Rect
  , _wndDragOffset :: Maybe Point
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

defaultWindowState :: WindowState
defaultWindowState = WindowState
  { _wndRect = Rect 100 100 200 200
  , _wndDragOffset = Nothing
  }

data DroppedState = DroppedState
  { _dsX :: Int
  , _dsY :: Int
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

data DraggableState = DraggableState
  { _ssDragX :: Int
  , _ssDragY :: Int
  , _ssDraggedInstance :: Instance
  , _ssDroppedInstance :: Instance
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

data State = State
  { _nodeState :: NodeState
  , _windowStates :: [WindowState]
  , _draggableState :: Maybe DraggableState
  , _draggableInstance :: Maybe InstanceState
  , _droppedState :: [DroppedState]
  , _instances :: [Instance]
  , _global :: A.Value
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

globalState :: A.Value
globalState = A.object
  [ "files" A..= A.toJSON defaultNodeState
  ]

defaultState = State
  { _nodeState = defaultNodeState
  , _windowStates = [defaultWindowState, defaultWindowState]
  , _draggableState = Nothing
  , _draggableInstance = Nothing
  , _droppedState = []
  , _instances = [] -- [ InstanceTree [Key "files"], InstanceTree [Key "files"] ]
  , _global = globalState
  }

data SongState = SongState deriving (Show, Generic, A.FromJSON, A.ToJSON)
data PlaylistState = PlaylistState deriving (Show, Generic, A.FromJSON, A.ToJSON)

data InstanceState = InstanceState
  { _instWindowState :: WindowState
  , _instInstance :: Instance
  } deriving (Show, Generic, A.FromJSON, A.ToJSON)

defaultInstanceState :: InstanceState
defaultInstanceState = InstanceState
  { _instWindowState = defaultWindowState
  , _instInstance = InstanceRect
  }

data Instance
  = InstanceRect
  | InstanceTree Path
  | InstanceSong Path
  | InstancePlaylist [Path]
  deriving (Show, Generic, A.FromJSON, A.ToJSON)

data PathSegment
  = Key Text
  | Index Int
  deriving (Show, Generic, A.FromJSON, A.ToJSON)

type Path = [PathSegment]

pathToLens :: A.ToJSON a => A.FromJSON a => Path -> AffineTraversal' A.Value a
pathToLens [] = castOptic A._JSON
pathToLens (Key k:ps) = A.key k % pathToLens ps
pathToLens (Index i:ps) = A.nth i % pathToLens ps

-- TH --------------------------------------------------------------------------

makeLenses ''Rect
makeLenses ''Point
makeLenses ''DroppedState
makeLenses ''WindowState
makeLenses ''NodeState
makeLenses ''DraggableState
makeLenses ''Instance
makeLenses ''InstanceState
makeLenses ''State
