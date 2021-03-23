{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (pack)

import Refract.DOM.Events
import Refract.DOM
import Refract

import Prelude hiding (div)

counter :: IO ()
counter = runDefault 3777 "Counter" 666 $ \_ -> state $ \st ->
  div []
    [ div [ onClick $ \_ st -> st + 1 ] [ text "+" ]
    , text (pack $ show st)
    , div [ onClick $ \_ st -> st - 1 ] [ text "-" ]
    ]

main :: IO ()
main = counter
