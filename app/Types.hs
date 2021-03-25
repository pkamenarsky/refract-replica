{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
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

-- Types -----------------------------------------------------------------------

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

data WindowState w = WindowState
  { _positionX :: Int
  , _positionY :: Int
  , _width :: Int
  , _height :: Int
  , _dragX :: Int
  , _dragY :: Int
  , _windowState :: w
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

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

data DroppedState = DroppedState
  { _dsX :: Int
  , _dsY :: Int
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

data DraggableState = DraggableState
  { _ssDragged :: Bool
  , _ssDragX :: Int
  , _ssDragY :: Int
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

defaultDraggableState :: DraggableState
defaultDraggableState = DraggableState
  { _ssDragged = False
  , _ssDragX = 0
  , _ssDragY = 0
  }

data State = State
  { _root :: NodeState
  , _windowStates :: [WindowState ()]
  , _draggableState :: DraggableState
  , _droppedState :: [DroppedState]
  , _instances :: [Instance]
  , _global :: A.Value
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

globalState :: A.Value
globalState = A.object
  [ "files" A..= A.toJSON defaultNodeState
  ]

defaultState = State
  { _root = defaultNodeState
  , _windowStates = [defaultWindowState, defaultWindowState]
  , _draggableState = defaultDraggableState
  , _droppedState = []
  , _instances = [ InstanceTree [Key "files"], InstanceTree [Key "files"] ]
  , _global = globalState
  }

data SongState = SongState deriving (Show, Generic, A.FromJSON, A.ToJSON)
data PlaylistState = PlaylistState deriving (Show, Generic, A.FromJSON, A.ToJSON)

data Instance
  = InstanceTree Path
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

makeLenses ''DroppedState
makeLenses ''WindowState
makeLenses ''NodeState
makeLenses ''DraggableState
makeLenses ''State
