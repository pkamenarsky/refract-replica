{-# LANGUAGE OverloadedStrings #-}

module Refract.DOM.Events where

import           Refract.DOM.Props (Props(Props), Prop(PropEvent))

import           Data.Aeson               ((.:), (.:?))
import qualified Data.Aeson               as A

import qualified Data.Text                as T

import           Replica.VDOM.Types       (DOMEvent(getDOMEvent))

-- TODO: return Maybe here
extractResult :: A.Result a -> a
extractResult (A.Success a) = a
extractResult (A.Error e)   = error e

data Target = Target
  { targetValue :: T.Text
  }

instance A.FromJSON Target where
  parseJSON (A.Object o) = Target
    <$> o .: "value"
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
  }

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
  }

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
  }

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
onBlur :: (BaseEvent -> st -> st) -> Props st
onBlur f = Props "onBlur" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
onClick :: (MouseEvent -> st -> st) -> Props st
onClick f = Props "onClick" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/focus
onFocus :: (BaseEvent -> st -> st) -> Props st
onFocus f = Props "onFocus" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dblclick
onDoubleClick :: (MouseEvent -> st -> st) -> Props st
onDoubleClick f = Props "onDblClick" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/input
onInput :: (BaseEvent -> st -> st) -> Props st
onInput f = Props "onInput" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/change
onChange :: (BaseEvent -> st -> st) -> Props st
onChange f = Props "onChange" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keydown
onKeyDown :: (KeyboardEvent -> st -> st) -> Props st
onKeyDown f = Props "onKeyDown" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keypress
onKeyPress :: (KeyboardEvent -> st -> st) -> Props st
onKeyPress f = Props "onKeyPress" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keyup
onKeyUp :: (KeyboardEvent -> st -> st) -> Props st
onKeyUp f = Props "onKeyUp" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseup
onMouseUp :: (MouseEvent -> st -> st) -> Props st
onMouseUp f = Props "onMouseUp" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mousedown
onMouseDown :: (MouseEvent -> st -> st) -> Props st
onMouseDown f = Props "onMouseDown" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseenter
onMouseEnter :: (MouseEvent -> st -> st) -> Props st
onMouseEnter f = Props "onMouseEnter" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseleave
onMouseLeave :: (MouseEvent -> st -> st) -> Props st
onMouseLeave f = Props "onMouseLeave" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseover
onMouseOver :: (MouseEvent -> st -> st) -> Props st
onMouseOver f = Props "onMouseOver" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseout
onMouseOut :: (MouseEvent -> st -> st) -> Props st
onMouseOut f = Props "onMouseOut" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragstart
onDragStart :: (MouseEvent -> st -> st) -> Props st
onDragStart f = Props "onDragStart" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragover
onDragOver :: (MouseEvent -> st -> st) -> Props st
onDragOver f = Props "onDragOver" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragend
onDragEnd :: (MouseEvent -> st -> st) -> Props st
onDragEnd f = Props "onDragEnd" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragenter
onDragEnter :: (MouseEvent -> st -> st) -> Props st
onDragEnter f = Props "onDragEnter" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragleave
onDragLeave :: (MouseEvent -> st -> st) -> Props st
onDragLeave f = Props "onDragLeave" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))

-- | https://developer.mozilla.org/en-US/docs/Web/Events/drag
onDrag :: (MouseEvent -> st -> st) -> Props st
onDrag f = Props "onDrag" (PropEvent (f . extractResult . A.fromJSON . getDOMEvent))
