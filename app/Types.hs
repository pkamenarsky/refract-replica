{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import qualified Data.Aeson as A
import qualified Data.Aeson.Optics as A
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
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

data InspectorState = InspectorState
  { _inspOpenNodes :: HS.HashSet Text
  , _inspMouseOverNode :: Maybe Text
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

defaultInspectorState :: InspectorState
defaultInspectorState = InspectorState
  { _inspOpenNodes = HS.empty
  , _inspMouseOverNode = Nothing
  }

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
  | InstanceInspector Path Path
  | InstanceSong Path
  | InstancePlaylist [Path]
  deriving (Show, Generic, A.FromJSON, A.ToJSON)

data InstanceState = InstanceState
  { _instName :: Text
  , _instWindowState :: WindowState
  , _instInstance :: Instance
  } deriving (Show, Generic, A.FromJSON, A.ToJSON)

defaultInstanceState :: InstanceState
defaultInstanceState = InstanceState
  { _instName = "X"
  , _instWindowState = defaultWindowState
  , _instInstance = InstanceRect
  }

data LayoutState
  = LayoutHSplit Int LayoutState LayoutState
  | LayoutVSplit Int LayoutState LayoutState
  | LayoutInstance Text Instance

defaultLayoutState :: LayoutState
defaultLayoutState = LayoutInstance "Inspector" (InstanceTree [Key "files"])

--------------------------------------------------------------------------------

data State = State
  { _draggedInstance :: Maybe InstanceState
  , _draggedWindow :: Maybe Text
  , _instances :: H.HashMap Text InstanceState
  , _global :: A.Value
  , _ctrlPressed :: Bool
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

globalState :: A.Value
globalState = A.object
  [ "files" A..= jsonToNodeState "root" (A.toJSON defaultNodeState) (NodeState "" False (NodeArray False []))
  , "song" A..= defaultSongState
  , "inspector" A..= defaultInspectorState
  ]

defaultState = State
  { _draggedInstance = Nothing
  , _draggedWindow = Nothing
  , _instances = H.fromList
    [ ("A", InstanceState "A" defaultWindowState (InstanceTree [Key "files"]))
    , ("B", InstanceState "B" defaultWindowState (InstanceTree [Key "files"]))
    , ("C", InstanceState "C" defaultWindowState (InstanceSong [Key "song"]))
    , ("D", InstanceState "D" defaultWindowState (InstanceInspector [Key "inspector"] []))
    ]
  , _global = globalState
  , _ctrlPressed = False
  }

-- TH --------------------------------------------------------------------------

makeLenses ''Rect
makeLenses ''Point
makeLenses ''WindowState
makePrisms ''NodeValue
makeLenses ''InspectorState
makeLenses ''NodeState
makeLenses ''Instance
makeLenses ''InstanceState
makeLenses ''State
