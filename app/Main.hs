{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.Trans.State (modify)
import qualified Control.Monad.Trans.State as ST
import Control.Monad.IO.Class (liftIO)

import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)

import Data.Text (Text, pack)
import Data.Tuple.Optics
import Data.Maybe (fromMaybe)
import Data.Maybe.Optics

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

import qualified Network.Wai.Handler.Replica as R

import Prelude hiding (div, span)

stateL :: Lens' st a -> (a -> Component st) -> Component st
stateL l f = state $ \st -> f (view l st)

toLens :: b -> AffineTraversal' a b -> Lens' a b
toLens d t = lens (fromMaybe d . preview t) (\a b -> set t b a)

unsafeIx :: Int -> Lens' [a] a
unsafeIx x = toLens (error "unsafeIx") $ ix x

counter' :: Lens' st Int -> Component st
counter' l = stateL l $ \st -> div []
  [ div [ onClick $ \_ -> modify $ over l $ \st -> st + 1 ] [ text "+" ]
  , text (pack $ show st)
  , div [ onClick $ \_ -> modify $ over l $ \st -> st - 1 ] [ text "-" ]
  ]

counter :: IO ()
counter = runDefault 3777 "Counter" [i | i <- [0..10]] [] $ \_ -> pure $ div []
  [ counter' (toLens 0 $ ix i)
  | i <- [0..10]
  ]

--------------------------------------------------------------------------------

data Node = Node
  { _nodeOpen :: Bool
  , _nodeName :: Text
  , _nodeChildren :: [Node]
  } deriving Show

makeLenses ''Node

showTree :: Int -> Lens' st Node -> Component st
showTree level l = stateL l $ \Node {..} -> div [ style padding ] $ mconcat
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
    padding = [ ("padding-left", pack (show $ level * 12) <> "px") ]

tree = Node False "/root" [ Node False "/home" [ Node False "/phil" [], Node False "/satan" [] ], Node False "/etc" [] ]

data RectState = RectState
  { _positionX :: Int
  , _positionY :: Int
  , _dragX :: Int
  , _dragY :: Int
  } deriving Show

makeLenses ''RectState

data State = State
  { _root :: Node
  , _rectState :: RectState
  } deriving Show

defaultState = State
  { _root = tree
  , _rectState = RectState
      { _positionX = 100
      , _positionY = 100
      , _dragX = 0
      , _dragY = 0
      }
  }

makeLenses ''State

draggable :: DragHandler st -> Lens' st RectState -> Component st
draggable startDrag l = stateL l $ \rs -> div
  [ css rs ]
  [ div
      [ header
      , onMouseDown $ \e -> startDrag e
          (\_ _ -> pure ())
          (\x y -> modify $ over l $ \rs' -> rs' { _dragX = x, _dragY = y })
          (modify $ over l $ \rs' -> rs' { _positionX = _positionX rs' + _dragX rs', _positionY = _positionY rs' + _dragY rs', _dragX = 0, _dragY = 0 })
      ] []
  ]
  where
    px x = pack (show x) <> "px"
    css RectState {..} = style
      [ ("position", "absolute")
      , ("left", px (_positionX + _dragX))
      , ("top", px (_positionY + _dragY))
      , ("width", "200px")
      , ("height", "200px")
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

type DragHandler st = 
     MouseEvent
  -> (Int -> Int -> ST.StateT st IO ()) -- ^ dragStarted
  -> (Int -> Int -> ST.StateT st IO ()) -- ^ dragDragged
  -> ST.StateT st IO () -- ^ dragFinished
  -> ST.StateT st IO ()

startDrag
  :: R.Context
  -> TChan (st -> IO st) -- ^ setState
  -> DragHandler st
startDrag ctx modStCh mouseEvent started dragged finished
  = liftIO $ dragAndDrop ctx writeState mouseEvent
  where
    action (DragStarted x y) = started x y
    action (DragDragged x y) = dragged x y
    action DragNone = finished

    writeState ds = atomically $ writeTChan modStCh $ ST.execStateT (action ds)

main :: IO ()
main = do
  ddChan <- newTChanIO
  runDefault 3777 "Tree" defaultState [ddChan] $ \ctx -> do

  pure $ let drag = startDrag ctx ddChan in
    div []
      [ state $ \st -> text (pack $ show st)
      , showTree 0 root
      , showTree 0 root
      , draggable drag rectState
      ]
