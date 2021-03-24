{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.Trans.State (modify)
import qualified Control.Monad.Trans.State as ST

import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)

import qualified Data.Aeson as A
import Data.Text (Text, pack)
import Data.Tuple.Optics
import Data.Maybe (fromMaybe)
import Data.Maybe.Optics

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

data WindowState = WindowState
  { _positionX :: Int
  , _positionY :: Int
  , _width :: Int
  , _height :: Int
  , _dragX :: Int
  , _dragY :: Int
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

makeLenses ''WindowState

defaultWindowState :: WindowState
defaultWindowState = WindowState
  { _positionX = 100
  , _positionY = 100
  , _width = 200
  , _height = 200
  , _dragX = 0
  , _dragY = 0
  }

window :: Component st -> DragHandler st -> Lens' st WindowState -> Component st
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
    px x = pack (show x) <> "px"
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

data SongState = SongState
  {
  }

song :: Component st
song = undefined


-- OS --------------------------------------------------------------------------

data State = State
  { _root :: NodeState
  , _windowStates :: [WindowState]
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)

makeLenses ''State

defaultState = State
  { _root = defaultNodeState
  , _windowStates = [defaultWindowState, defaultWindowState]
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
