{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.Trans.State (modify)
import Control.Monad.IO.Class (liftIO)

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
counter = runDefault 3777 "Counter" [i | i <- [0..10]] $ \_ -> pure $ div []
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
        [ text $ (if _nodeOpen then "-" else "+") <> _nodeName ] ]

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

data State = State
  { _root :: Node
  , _dragState :: DragState
  } deriving Show

defaultState = State
  { _root = tree
  , _dragState = DragNone
  }

makeLenses ''State

draggable :: (MouseEvent -> IO ()) -> Lens' st DragState -> Component st
draggable drag l = stateL l $ \ds -> div
  [ css ds
  , onMouseDown $ \e -> liftIO (drag e)
  ] []
  where
    px x = pack (show x) <> "px"
    css ds = style
      [ ("position", "absolute")
      , ("left", px (200 + ox))
      , ("top", px (200 + oy))
      , ("width", "200px")
      , ("height", "200px")
      , ("backgroundColor", "#777")
      ]
      where
        (ox, oy) = case ds of
          DragStarted x y -> (x, y)
          DragDragged x y -> (x, y)
          DragNone -> (0, 0)

main :: IO ()
main = runDefault 3777 "Tree" defaultState $ \ctx -> do

  pure $ state' $ \st setState -> let drag = dragAndDrop ctx (\ds -> setState (set dragState ds st) >> print ds) in
    div []
      [ state $ \st -> text (pack $ show st)
      , showTree 0 root
      , showTree 0 root
      , draggable drag dragState
      ]
