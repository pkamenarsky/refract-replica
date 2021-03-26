{-# LANGUAGE OverloadedStrings #-}

module Refract.DOM.Events where

import           Control.Monad.Fix            (mfix)
import           Control.Monad.IO.Class       (liftIO)
import qualified Control.Monad.Trans.State as ST

import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (TChan, writeTChan)

import           Refract.DOM.Props            (Props(Props), Prop(PropEvent))

import           Data.Aeson                   ((.:), (.:?))
import qualified Data.Aeson                as A

import qualified Data.Text                 as T

import           Replica.VDOM.Types           (DOMEvent(getDOMEvent))

import qualified Network.Wai.Handler.Replica as R

-- TODO: return Maybe here
extractResult :: A.Result a -> a
extractResult (A.Success a) = a
extractResult (A.Error e)   = error e

data Target = Target
  { targetValue :: Maybe T.Text
  } deriving Show

instance A.FromJSON Target where
  parseJSON (A.Object o) = Target
    <$> o .:? "value"
  parseJSON _ = fail "Expected object"

data BaseEvent = BaseEvent
  { bubbles          :: !Bool
  , cancelable       :: !Bool
  , composed         :: !Bool
  , currentTarget    :: !Target
  , defaultPrevented :: !Bool
  , eventPhase       :: !Int
  , target           :: !Target
  , timeStamp        :: !Double
  , eventType        :: !T.Text
  , isTrusted        :: !Bool
  } deriving Show

instance A.FromJSON BaseEvent where
  parseJSON (A.Object o) = BaseEvent
    <$> o .: "bubbles"
    <*> o .: "cancelable"
    <*> o .: "composed"
    <*> o .: "currentTarget"
    <*> o .: "defaultPrevented"
    <*> o .: "eventPhase"
    <*> o .: "target"
    <*> o .: "timeStamp"
    <*> o .: "type"
    <*> o .: "isTrusted"
  parseJSON _ = fail "Expected object"

data MouseEvent = MouseEvent
  { mouseBaseEvent     :: !BaseEvent
  , mouseAltKey        :: !Bool
  , mouseButton        :: !Int
  , mouseButtons       :: !Int
  , mouseClientX       :: !Int
  , mouseClientY       :: !Int
  , mouseCtrlKey       :: !Bool
  , mouseMetaKey       :: !Bool
  , mouseMovementX     :: !Int
  , mouseMovementY     :: !Int
  , mouseRegion        :: !(Maybe T.Text)
  , mouseRelatedTarget :: !(Maybe Target)
  , mouseScreenX       :: !Int
  , mouseScreenY       :: !Int
  , mouseShiftKey      :: !Bool
  } deriving Show

instance A.FromJSON MouseEvent where
  parseJSON obj@(A.Object o) = MouseEvent
    <$> A.parseJSON obj
    <*> o .:  "altKey"
    <*> o .:  "button"
    <*> o .:  "buttons"
    <*> o .:  "clientX"
    <*> o .:  "clientY"
    <*> o .:  "ctrlKey"
    <*> o .:  "metaKey"
    <*> o .:  "movementX"
    <*> o .:  "movementY"
    <*> o .:? "region"
    <*> o .:? "relatedTarget"
    <*> o .:  "screenX"
    <*> o .:  "screenY"
    <*> o .:  "shiftKey"
  parseJSON _ = fail "Expected object"

data KeyboardEvent = KeyboardEvent
  { kbdBaseEvent   :: !BaseEvent
  , kbdAltKey      :: !Bool
  , kbdCtrlKey     :: !Bool
  , kbdIsComposing :: !Bool
  , kbdKey         :: !T.Text
  , kbdLocation    :: !Int
  , kbdMetaKey     :: !Bool
  , kbdRepeat      :: !Bool
  , kbdShiftKey    :: !Bool
  } deriving Show

instance A.FromJSON KeyboardEvent where
  parseJSON obj@(A.Object o) = KeyboardEvent
    <$> A.parseJSON obj
    <*> o .: "altKey"
    <*> o .: "ctrlKey"
    <*> o .: "isComposing"
    <*> o .: "key"
    <*> o .: "location"
    <*> o .: "metaKey"
    <*> o .: "repeat"
    <*> o .: "shiftKey"
  parseJSON _ = fail "Expected object"

--------------------------------------------------------------------------------

-- | `blur` event defined with custom options
--
-- <https://developer.mozilla.org/en-US/docs/Web/Events/blur>
--
onBlur :: (BaseEvent -> ST.StateT st IO ()) -> Props st
onBlur f = Props "onBlur" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
onClick :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onClick f = Props "onClick" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/focus
onFocus :: (BaseEvent -> ST.StateT st IO ()) -> Props st
onFocus f = Props "onFocus" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dblclick
onDoubleClick :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onDoubleClick f = Props "onDblClick" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/input
onInput :: (BaseEvent -> ST.StateT st IO ()) -> Props st
onInput f = Props "onInput" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/change
onChange :: (BaseEvent -> ST.StateT st IO ()) -> Props st
onChange f = Props "onChange" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keydown
onKeyDown :: (KeyboardEvent -> ST.StateT st IO ()) -> Props st
onKeyDown f = Props "onKeyDown" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keypress
onKeyPress :: (KeyboardEvent -> ST.StateT st IO ()) -> Props st
onKeyPress f = Props "onKeyPress" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keyup
onKeyUp :: (KeyboardEvent -> ST.StateT st IO ()) -> Props st
onKeyUp f = Props "onKeyUp" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseup
onMouseUp :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onMouseUp f = Props "onMouseUp" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mousedown
onMouseDown :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onMouseDown f = Props "onMouseDown" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseenter
onMouseEnter :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onMouseEnter f = Props "onMouseEnter" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseleave
onMouseLeave :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onMouseLeave f = Props "onMouseLeave" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseover
onMouseOver :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onMouseOver f = Props "onMouseOver" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseout
onMouseOut :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onMouseOut f = Props "onMouseOut" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragstart
onDragStart :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onDragStart f = Props "onDragStart" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragover
onDragOver :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onDragOver f = Props "onDragOver" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragend
onDragEnd :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onDragEnd f = Props "onDragEnd" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragenter
onDragEnter :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onDragEnter f = Props "onDragEnter" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragleave
onDragLeave :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onDragLeave f = Props "onDragLeave" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/drag
onDrag :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onDrag f = Props "onDrag" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

--------------------------------------------------------------------------------

data DragState = DragStarted Int Int | DragDragged Int Int | DragNone
  deriving (Eq, Show)

dragAndDrop :: R.Context -> (DragState -> IO ()) -> MouseEvent -> IO ()
dragAndDrop ctx cb event = do
  jsCb <- mfix $ \jsCb -> do
    jsCb <- R.registerCallback ctx (dragged jsCb)
    pure jsCb

  cb (DragStarted 0 0)

  R.call ctx jsCb js
  where
    js = "var drag = function(e) { \n\
        \   callCallback(arg, [e.clientX, e.clientY]); \n\
        \ }; \n\
        \ var up = function(e) { \n\
        \   console.log('up', e); \n\
        \   window.removeEventListener('mousemove', drag); \n\
        \   window.removeEventListener('mouseup', up); \n\
        \   callCallback(arg, null); \n\
        \ }; \n\
        \ window.addEventListener('mousemove', drag); \n\
        \ window.addEventListener('mouseup', up); \n\
        \"

    dragged :: R.Callback -> Maybe (Int, Int) -> IO ()
    dragged jsCb xy@Nothing = do
      R.unregisterCallback ctx jsCb
      cb DragNone
    dragged jsCb xy@(Just (x, y)) = cb $ DragDragged
      (x - mouseClientX event)
      (y - mouseClientY event)

type StartDrag st = 
     MouseEvent
  -> (Int -> Int -> ST.StateT st IO ()) -- ^ dragStarted
  -> (Int -> Int -> ST.StateT st IO ()) -- ^ dragDragged
  -> ST.StateT st IO () -- ^ dragFinished
  -> ST.StateT st IO ()

startDrag
  :: R.Context
  -> TChan (st -> IO st) -- ^ setState
  -> StartDrag st
startDrag ctx modStCh mouseEvent started dragged finished
  = liftIO $ dragAndDrop ctx writeState mouseEvent
  where
    action (DragStarted x y) = started x y
    action (DragDragged x y) = dragged x y
    action DragNone = finished

    writeState ds = atomically $ writeTChan modStCh $ ST.execStateT (action ds)
