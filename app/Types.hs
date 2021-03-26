{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import qualified Data.Aeson as A
import qualified Data.Aeson.Optics as A
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
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
  | NodeArray Bool [NodeState]
  | NodeBool Bool
  deriving (Show, Generic, A.ToJSON, A.FromJSON)

data NodeState = NodeState
  { _nodeName :: Text
  , _nodeMouseOver :: Bool
  , _nodeChildren :: NodeValue
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

defaultNodeState :: NodeState
defaultNodeState = NodeState "root" False $ NodeArray False
  [ NodeState "home" False $ NodeArray False
      [ NodeState "phil" False (NodeString "666")
      , NodeState "satan" False (NodeString "666")
      ]
  , NodeState "etc" False $ NodeArray False []
  ]

jsonToNodeState :: Text -> A.Value -> NodeState -> NodeState
jsonToNodeState nn (A.Object m) (NodeState _ mo (NodeArray o children)) = NodeState nn mo $ NodeArray o
  [ jsonToNodeState k v $ fromMaybe (NodeState k False (NodeArray False [])) (H.lookup k chm)
  | (k, v) <- H.toList m
  ]
  where
    chm = H.fromList [ (k, v) | v@(NodeState k _ _) <- children ]
jsonToNodeState nn v@(A.Object m) _ = jsonToNodeState nn v (NodeState nn False (NodeArray False []))

jsonToNodeState nn (A.Array a) (NodeState _ mo (NodeArray o children)) = NodeState nn mo $ NodeArray o
  [ jsonToNodeState (pack $ show i) v $ fromMaybe (NodeState (pack $ show i) False (NodeArray False [])) ch
  | ((i, v), ch) <- zip (zip [0..] (V.toList a)) (map Just children <> repeat Nothing)
  ]
jsonToNodeState nn v@(A.Array m) _ = jsonToNodeState nn v (NodeState nn False (NodeArray False []))

jsonToNodeState nn (A.String s) (NodeState _ mo (NodeString t)) = NodeState nn mo (NodeString s)
jsonToNodeState nn (A.String s) _ = NodeState nn False (NodeString s)
jsonToNodeState nn (A.Number s) (NodeState _ mo (NodeString t)) = NodeState nn mo (NodeNumber $ fromRational $ toRational s)
jsonToNodeState nn (A.Number s) _ = NodeState nn False (NodeNumber $ fromRational $ toRational s)
jsonToNodeState nn (A.Bool s) (NodeState _ mo (NodeString t)) = NodeState nn mo (NodeBool s)
jsonToNodeState nn (A.Bool s) _ = NodeState nn False (NodeBool s)
jsonToNodeState nn A.Null (NodeState _ mo (NodeString t)) = NodeState nn mo (NodeString "null")
jsonToNodeState nn A.Null _ = NodeState nn False (NodeString "null")

data WindowState = WindowState
  { _wndRect :: Rect
  , _wndDragOffset :: Maybe Point
  , _wndTitleBar :: Bool
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

defaultWindowState :: WindowState
defaultWindowState = WindowState
  { _wndRect = Rect 100 100 500 500
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
  [ "files" A..= jsonToNodeState "root" (A.toJSON defaultNodeState) (NodeState "" False (NodeArray False []))
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
