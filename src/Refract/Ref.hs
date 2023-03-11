{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Refract.Ref where

import Data.IORef
import Data.Text

import qualified Control.Monad.Writer as W

import Prelude hiding (div)


data Ref a = Ref (IORef a)

-- withRef :: Ref a -> (a -> (a -> Update ()) -> Render r) -> Render r
-- withRef (Ref r) f = do
--   v <- Render (readIORef r)
--   f v (Update . writeIORef r)

getRef' :: Ref a -> (a -> Render r) -> Render r
getRef' (Ref r) f = Render (readIORef r) >>= f

getRef :: Ref a -> Render a
getRef (Ref r) = Render $ readIORef r

setRef :: Ref a -> a -> Update ()
setRef (Ref r) a = Update $ writeIORef r a

newtype Render a = Render (IO a)
  deriving (Functor, Applicative, Monad)

newtype Update a = Update (IO a)
  deriving (Functor, Applicative, Monad)

newtype Handler a = Handler (W.WriterT [Update ()] IO a)
  deriving (Functor, Applicative, Monad)

data Event
data DOM
data Props

onClick :: (Event -> Update ()) -> Props
onClick = undefined

label :: Text -> Render DOM
label = undefined

div :: [Props] -> [Render DOM] -> Render DOM
div = undefined

test :: Ref Text -> Ref Int -> Render DOM
test tref iref = div []
  [ div
      [ onClick $ \e -> do
          setRef tref "bla"
          setRef iref 5
      ]
      [ do
          t <- getRef tref
          label t
      ]
  , getRef' tref label
  ]
