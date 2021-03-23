{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

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
  [ div [ onClick $ \_ -> over l $ \st -> st + 1 ] [ text "+" ]
  , text (pack $ show st)
  , div [ onClick $ \_ -> over l $ \st -> st - 1 ] [ text "-" ]
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

data DragState

dragDrop :: Lens' st DragState -> Component st
dragDrop = undefined

showTree :: Int -> Lens' st Node -> Component st
showTree level l = stateL l $ \Node {..} -> div [ style padding ] $ mconcat
  [ [ span
        [ onClick $ \_ -> over (l % nodeOpen) not ]
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

main :: IO ()
main = runDefault 3777 "Tree" tree $ \ctx -> do
  registerDragAndDrop ctx print

  pure $ div []
    [ state $ \st -> text (pack $ show st)
    , showTree 0 (castOptic simple)
    , showTree 0 (castOptic simple)
    ]
