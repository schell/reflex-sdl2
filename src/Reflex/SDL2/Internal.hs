-- | This module holds low-level implementation details of reflex-sdl2
-- that are not meant to be dealt with by the user of the library.
-- None-the-less these details are exported here just in case you
-- need them :)
module Reflex.SDL2.Internal where

import           Control.Concurrent (MVar)
import           Data.Word          (Word32)
import           Reflex             (Event)
import           SDL                hiding (Event)

------------------------------------------------------------------------------
-- | Holds a slot of 'Event' for each kind of SDL2 event plus a couple extras:
--
-- An event for *any* SDL2 event payload.
--
-- An event for reflex's post network build event.
--
-- An event for each frame tick.
data SystemEvents t = SystemEvents
  { sysPostBuildEvent                 :: Event t ()
  -- ^ Fired just after the FRP network is built.
  , sysTicksEvent                     :: Event t Word32
  -- ^ Fired once per frame tick, contains the number of
  -- milliseconds since SDL library initialization.
  , sysAnySDLEvent                    :: Event t EventPayload
  -- ^ Fired when SDL receives any event.
  , sysWindowShownEvent               :: Event t WindowShownEventData
  , sysWindowHiddenEvent              :: Event t WindowHiddenEventData
  , sysWindowExposedEvent             :: Event t WindowExposedEventData
  , sysWindowMovedEvent               :: Event t WindowMovedEventData
  , sysWindowResizedEvent             :: Event t WindowResizedEventData
  , sysWindowSizeChangedEvent         :: Event t WindowSizeChangedEventData
  , sysWindowMinimizedEvent           :: Event t WindowMinimizedEventData
  , sysWindowMaximizedEvent           :: Event t WindowMaximizedEventData
  , sysWindowRestoredEvent            :: Event t WindowRestoredEventData
  , sysWindowGainedMouseFocusEvent    :: Event t WindowGainedMouseFocusEventData
  , sysWindowLostMouseFocusEvent      :: Event t WindowLostMouseFocusEventData
  , sysWindowGainedKeyboardFocusEvent :: Event t WindowGainedKeyboardFocusEventData
  , sysWindowLostKeyboardFocusEvent   :: Event t WindowLostKeyboardFocusEventData
  , sysWindowClosedEvent              :: Event t WindowClosedEventData
  , sysKeyboardEvent                  :: Event t KeyboardEventData
  , sysTextEditingEvent               :: Event t TextEditingEventData
  , sysTextInputEvent                 :: Event t TextInputEventData
  , sysKeymapChangedEvent             :: Event t ()
  , sysMouseMotionEvent               :: Event t MouseMotionEventData
  , sysMouseButtonEvent               :: Event t MouseButtonEventData
  , sysMouseWheelEvent                :: Event t MouseWheelEventData
  , sysJoyAxisEvent                   :: Event t JoyAxisEventData
  , sysJoyBallEvent                   :: Event t JoyBallEventData
  , sysJoyHatEvent                    :: Event t JoyHatEventData
  , sysJoyButtonEvent                 :: Event t JoyButtonEventData
  , sysJoyDeviceEvent                 :: Event t JoyDeviceEventData
  , sysControllerAxisEvent            :: Event t ControllerAxisEventData
  , sysControllerButtonEvent          :: Event t ControllerButtonEventData
  , sysControllerDeviceEvent          :: Event t ControllerDeviceEventData
  , sysAudioDeviceEvent               :: Event t AudioDeviceEventData
  , sysQuitEvent                      :: Event t ()
  , sysUserEvent                      :: Event t UserEventData
  , sysSysWMEvent                     :: Event t SysWMEventData
  , sysTouchFingerEvent               :: Event t TouchFingerEventData
  , sysTouchFingerMotionEvent         :: Event t TouchFingerMotionEventData
  , sysMultiGestureEvent              :: Event t MultiGestureEventData
  , sysDollarGestureEvent             :: Event t DollarGestureEventData
  , sysDropEvent                      :: Event t DropEventData
  , sysClipboardUpdateEvent           :: Event t ()
  , sysUnknownEvent                   :: Event t UnknownEventData
  , sysQuitVar                        :: MVar ()
  -- ^ A var to sync quitting.
  }
