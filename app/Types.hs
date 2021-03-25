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

data PathSegment
  = Key Text
  | Index Int
  deriving (Show, Generic, A.FromJSON, A.ToJSON)

type Path = [PathSegment]

pathToLens :: A.ToJSON a => A.FromJSON a => Path -> AffineTraversal' A.Value a
pathToLens [] = castOptic A._JSON
pathToLens (Key k:ps) = A.key k % pathToLens ps
pathToLens (Index i:ps) = A.nth i % pathToLens ps

--------------------------------------------------------------------------------

data NodeValue
  = NodeString Text
  | NodeNumber Double
  | NodeArray [NodeState]
  deriving (Show, Generic, A.ToJSON, A.FromJSON)

data NodeState = NodeState
  { _nodeOpen :: Bool
  , _nodeName :: Text
  , _nodeChildren :: NodeValue
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

defaultNodeState :: NodeState
defaultNodeState = NodeState False "/root" $ NodeArray
  [ NodeState False "/home" $ NodeArray
      [ NodeState False "/phil" (NodeString "666")
      , NodeState False "/satan" (NodeString "666")
      ]
  , NodeState False "/etc" $ NodeArray []
  ]

data WindowState = WindowState
  { _wndRect :: Rect
  , _wndDragOffset :: Maybe Point
  , _wndTitleBar :: Bool
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

defaultWindowState :: WindowState
defaultWindowState = WindowState
  { _wndRect = Rect 100 100 200 200
  , _wndDragOffset = Nothing
  , _wndTitleBar = True 
  }

data SongState = SongState deriving (Show, Generic, A.FromJSON, A.ToJSON)

defaultSongState :: SongState
defaultSongState = SongState

data PlaylistState = PlaylistState deriving (Show, Generic, A.FromJSON, A.ToJSON)

data Instance
  = InstanceRect
  | InstanceTree Path
  | InstanceSong Path
  | InstancePlaylist [Path]
  deriving (Show, Generic, A.FromJSON, A.ToJSON)

data InstanceState = InstanceState
  { _instWindowState :: WindowState
  , _instInstance :: Instance
  } deriving (Show, Generic, A.FromJSON, A.ToJSON)

defaultInstanceState :: InstanceState
defaultInstanceState = InstanceState
  { _instWindowState = defaultWindowState
  , _instInstance = InstanceRect
  }

data State = State
  { _draggedInstance :: Maybe InstanceState
  , _instances :: [InstanceState]
  , _global :: A.Value
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

globalState :: A.Value
globalState = A.object
  [ "files" A..= defaultNodeState
  , "song" A..= defaultSongState
  ]

defaultState = State
  { _draggedInstance = Nothing
  , _instances =
    [ InstanceState defaultWindowState (InstanceTree [Key "files"])
    , InstanceState defaultWindowState (InstanceTree [Key "files"])
    , InstanceState defaultWindowState (InstanceSong [Key "song"])
    ]
  , _global = globalState
  }

-- TH --------------------------------------------------------------------------

makeLenses ''Rect
makeLenses ''Point
makeLenses ''WindowState
makePrisms ''NodeValue
makeLenses ''NodeState
makeLenses ''Instance
makeLenses ''InstanceState
makeLenses ''State
