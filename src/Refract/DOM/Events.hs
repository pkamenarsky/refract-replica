{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Refract.DOM.Events where

import           Control.Monad.Fix            (mfix)
import           Control.Monad.IO.Class       (liftIO)
import qualified Control.Monad.Trans.State as ST

import           Control.Concurrent.MVar
import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (TChan, writeTChan)

import           Refract.DOM.Props            (Props, Prop(PropEvent), props)

import           Data.Aeson                   ((.:), (.:?))
import qualified Data.Aeson                as A
import           Data.IORef

import qualified Data.Text                 as T

import           Replica.VDOM.Types           (DOMEvent(getDOMEvent), EventOptions(EventOptions, evtCapture, evtPreventDefault, evtStopPropagation))

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
  parseJSON _ = pure $ Target Nothing

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

defaultOptions :: EventOptions
defaultOptions = EventOptions
  { evtCapture = False
  , evtPreventDefault = False
  , evtStopPropagation = False
  }

-- | `blur` event defined with custom options
--
-- <https://developer.mozilla.org/en-US/docs/Web/Events/blur>
--
onBlur :: (BaseEvent -> ST.StateT st IO ()) -> Props st
onBlur f = props "onBlur" (PropEvent defaultOptions (f . extractResult . A.fromJSON . getDOMEvent))

onBlurWithOptions :: EventOptions -> (BaseEvent -> ST.StateT st IO ()) -> Props st
onBlurWithOptions opts f = props "onBlur" (PropEvent opts (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
onClick :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onClick f = props "onClick" (PropEvent defaultOptions (f . extractResult . A.fromJSON . getDOMEvent))

onClickWithOptions :: EventOptions -> (MouseEvent -> ST.StateT st IO ()) -> Props st
onClickWithOptions opts f = props "onClick" (PropEvent opts (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/focus
onFocus :: (BaseEvent -> ST.StateT st IO ()) -> Props st
onFocus f = props "onFocus" (PropEvent defaultOptions (f . extractResult . A.fromJSON . getDOMEvent))

onFocusWithOptions :: EventOptions -> (BaseEvent -> ST.StateT st IO ()) -> Props st
onFocusWithOptions opts f = props "onFocus" (PropEvent opts (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dblclick
onDoubleClick :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onDoubleClick f = props "onDblClick" (PropEvent defaultOptions (f . extractResult . A.fromJSON . getDOMEvent))

onDoubleClickWithOptions :: EventOptions -> (MouseEvent -> ST.StateT st IO ()) -> Props st
onDoubleClickWithOptions opts f = props "onDblClick" (PropEvent opts (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/input
onInput :: (BaseEvent -> ST.StateT st IO ()) -> Props st
onInput f = props "onInput" (PropEvent defaultOptions (f . extractResult . A.fromJSON . getDOMEvent))

onInputWithOptions :: EventOptions -> (BaseEvent -> ST.StateT st IO ()) -> Props st
onInputWithOptions opts f = props "onInput" (PropEvent opts (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/change
onChange :: (BaseEvent -> ST.StateT st IO ()) -> Props st
onChange f = props "onChange" (PropEvent defaultOptions (f . extractResult . A.fromJSON . getDOMEvent))

onChangeWithOptions :: EventOptions -> (BaseEvent -> ST.StateT st IO ()) -> Props st
onChangeWithOptions opts f = props "onChange" (PropEvent opts (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keydown
onKeyDown :: (KeyboardEvent -> ST.StateT st IO ()) -> Props st
onKeyDown f = props "onKeyDown" (PropEvent defaultOptions (f . extractResult . A.fromJSON . getDOMEvent))

onKeyDownWithOptions :: EventOptions -> (KeyboardEvent -> ST.StateT st IO ()) -> Props st
onKeyDownWithOptions opts f = props "onKeyDown" (PropEvent opts (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keypress
onKeyPress :: (KeyboardEvent -> ST.StateT st IO ()) -> Props st
onKeyPress f = props "onKeyPress" (PropEvent defaultOptions (f . extractResult . A.fromJSON . getDOMEvent))

onKeyPressWithOptions :: EventOptions -> (KeyboardEvent -> ST.StateT st IO ()) -> Props st
onKeyPressWithOptions opts f = props "onKeyPress" (PropEvent opts (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keyup
onKeyUp :: (KeyboardEvent -> ST.StateT st IO ()) -> Props st
onKeyUp f = props "onKeyUp" (PropEvent defaultOptions (f . extractResult . A.fromJSON . getDOMEvent))

onKeyUpWithOptions :: EventOptions -> (KeyboardEvent -> ST.StateT st IO ()) -> Props st
onKeyUpWithOptions opts f = props "onKeyUp" (PropEvent opts (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseup
onMouseUp :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onMouseUp f = props "onMouseUp" (PropEvent defaultOptions (f . extractResult . A.fromJSON . getDOMEvent))

onMouseUpWithOptions :: EventOptions -> (MouseEvent -> ST.StateT st IO ()) -> Props st
onMouseUpWithOptions opts f = props "onMouseUp" (PropEvent opts (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mousedown
onMouseDown :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onMouseDown f = props "onMouseDown" (PropEvent defaultOptions (f . extractResult . A.fromJSON . getDOMEvent))

onMouseDownWithOptions :: EventOptions -> (MouseEvent -> ST.StateT st IO ()) -> Props st
onMouseDownWithOptions opts f = props "onMouseDown" (PropEvent opts (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mousemove
onMouseMove :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onMouseMove f = props "onMouseMove" (PropEvent defaultOptions (f . extractResult . A.fromJSON . getDOMEvent))

onMouseMoveWithOptions :: EventOptions -> (MouseEvent -> ST.StateT st IO ()) -> Props st
onMouseMoveWithOptions opts f = props "onMouseMove" (PropEvent opts (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseenter
onMouseEnter :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onMouseEnter f = props "onMouseEnter" (PropEvent defaultOptions (f . extractResult . A.fromJSON . getDOMEvent))

onMouseEnterWithOptions :: EventOptions -> (MouseEvent -> ST.StateT st IO ()) -> Props st
onMouseEnterWithOptions opts f = props "onMouseEnter" (PropEvent opts (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseleave
onMouseLeave :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onMouseLeave f = props "onMouseLeave" (PropEvent defaultOptions (f . extractResult . A.fromJSON . getDOMEvent))

onMouseLeaveWithOptions :: EventOptions -> (MouseEvent -> ST.StateT st IO ()) -> Props st
onMouseLeaveWithOptions opts f = props "onMouseLeave" (PropEvent opts (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseover
onMouseOver :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onMouseOver f = props "onMouseOver" (PropEvent defaultOptions (f . extractResult . A.fromJSON . getDOMEvent))

onMouseOverWithOptions :: EventOptions -> (MouseEvent -> ST.StateT st IO ()) -> Props st
onMouseOverWithOptions opts f = props "onMouseOver" (PropEvent opts (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseout
onMouseOut :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onMouseOut f = props "onMouseOut" (PropEvent defaultOptions (f . extractResult . A.fromJSON . getDOMEvent))

onMouseOutWithOptions :: EventOptions -> (MouseEvent -> ST.StateT st IO ()) -> Props st
onMouseOutWithOptions opts f = props "onMouseOut" (PropEvent opts (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragstart
onDragStart :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onDragStart f = props "onDragStart" (PropEvent defaultOptions (f . extractResult . A.fromJSON . getDOMEvent))

onDragStartWithOptions :: EventOptions -> (MouseEvent -> ST.StateT st IO ()) -> Props st
onDragStartWithOptions opts f = props "onDragStart" (PropEvent opts (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragover
onDragOver :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onDragOver f = props "onDragOver" (PropEvent defaultOptions (f . extractResult . A.fromJSON . getDOMEvent))

onDragOverWithOptions :: EventOptions -> (MouseEvent -> ST.StateT st IO ()) -> Props st
onDragOverWithOptions opts f = props "onDragOver" (PropEvent opts (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragend
onDragEnd :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onDragEnd f = props "onDragEnd" (PropEvent defaultOptions (f . extractResult . A.fromJSON . getDOMEvent))

onDragEndWithOptions :: EventOptions -> (MouseEvent -> ST.StateT st IO ()) -> Props st
onDragEndWithOptions opts f = props "onDragEnd" (PropEvent opts (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragenter
onDragEnter :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onDragEnter f = props "onDragEnter" (PropEvent defaultOptions (f . extractResult . A.fromJSON . getDOMEvent))

onDragEnterWithOptions :: EventOptions -> (MouseEvent -> ST.StateT st IO ()) -> Props st
onDragEnterWithOptions opts f = props "onDragEnter" (PropEvent opts (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragleave
onDragLeave :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onDragLeave f = props "onDragLeave" (PropEvent defaultOptions (f . extractResult . A.fromJSON . getDOMEvent))

onDragLeaveWithOptions :: EventOptions -> (MouseEvent -> ST.StateT st IO ()) -> Props st
onDragLeaveWithOptions opts f = props "onDragLeave" (PropEvent opts (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/drag
onDrag :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onDrag f = props "onDrag" (PropEvent defaultOptions (f . extractResult . A.fromJSON . getDOMEvent))

onDragWithOptions :: EventOptions -> (MouseEvent -> ST.StateT st IO ()) -> Props st
onDragWithOptions opts f = props "onDrag" (PropEvent opts (f . extractResult . A.fromJSON . getDOMEvent))

--------------------------------------------------------------------------------

keyEvents :: R.Context -> (KeyboardEvent -> IO ()) -> (KeyboardEvent -> IO ()) -> IO ()
keyEvents ctx keyDown keyUp = do
  jsKeyDownCb <- R.registerCallback ctx keyDown
  jsKeyUpCb <- R.registerCallback ctx keyUp

  R.call ctx (jsKeyDownCb, jsKeyUpCb) js
  where
    js = "var keydown = function(e) { \n\
        \   callCallback(arg[0], JSON.parse(stringifyEvent(e)), true); \n\
        \   if (e.key == 'Alt') e.preventDefault(); \n\
        \ }; \n\
        \ var keyup = function(e) { \n\
        \   callCallback(arg[1], JSON.parse(stringifyEvent(e)), true); \n\
        \   if (e.key == 'Alt') e.preventDefault(); \n\
        \ }; \n\
        \ window.addEventListener('keydown', keydown); \n\
        \ window.addEventListener('keyup', keyup); \n\
        \"

type StartTrackedDrag st = forall a.
     MouseEvent
  -> (ST.StateT st IO a) -- ^ dragStarted
  -> (a -> Bool -> Int -> Int -> ST.StateT st IO ()) -- ^ dragDragged
  -> (ST.StateT st IO ()) -- ^ dragFinished
  -> ST.StateT st IO ()

-- TODO: pointer capture instead of windows
startTrackedDrag
  :: R.Context
  -> TChan (st -> IO st) -- ^ setState
  -> StartTrackedDrag st
startTrackedDrag ctx modStCh mouseEvent dragStarted dragDragged dragFinished = do
  ref <- liftIO $ newIORef undefined

  jsCb <- liftIO $ mfix $ \jsCb -> do
    jsCb <- R.registerCallback ctx (dragged ref jsCb)
    pure jsCb

  liftIO $ writeState $ do
    a <- dragStarted
    liftIO $ writeIORef ref a

  liftIO $ R.call ctx jsCb js
  where
    js = "let first_drag = true; \n\
        \ let drag = function(e) { \n\
        \   callCallback(arg, [first_drag, e.clientX, e.clientY], true); \n\
        \   first_drag = false; \n\
        \   e.preventDefault(); \n\
        \ }; \n\
        \ let up = function(e) { \n\
        \   window.removeEventListener('mousemove', drag); \n\
        \   window.removeEventListener('mouseup', up); \n\
        \   const element = document.elementFromPoint(e.clientX, e.clientY); \n\
        \   const event = new CustomEvent('trackeddragend', { bubbles: true }); \n\
        \   element.dispatchEvent(event); \n\
        \   callCallback(arg, null, true); \n\
        \ }; \n\
        \ window.addEventListener('mousemove', drag); \n\
        \ window.addEventListener('mouseup', up); \n\
        \"

    dragged _ jsCb xy@Nothing = do
      liftIO $ R.unregisterCallback ctx jsCb
      writeState dragFinished
    dragged ref jsCb xy@(Just (firstDrag, x, y)) = writeState $ do
      a <- liftIO $ readIORef ref
      dragDragged a
        firstDrag
        (x - mouseClientX mouseEvent)
        (y - mouseClientY mouseEvent)

    writeState ds = atomically $ writeTChan modStCh $ ST.execStateT ds

onTrackedDragEnd :: (MouseEvent -> ST.StateT st IO ()) -> Props st
onTrackedDragEnd f = props "onTrackedDragEnd" (PropEvent defaultOptions (f . extractResult . A.fromJSON . getDOMEvent))
