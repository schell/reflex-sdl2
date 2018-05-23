{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
module Reflex.SDL2.Class where


import           Control.Concurrent        (MVar)
import           Control.Monad.Reader      (ReaderT)
import           Control.Monad.Trans       (lift)
import           Data.Word                 (Word32)
import           Reflex                    (Event, Reflex)
import           Reflex.DynamicWriter.Base (DynamicWriterT)
import           SDL                       hiding (Event)


class (Reflex t, Monad m) => HasSDL2Events t m | m -> t where
  getTicksEvent :: m (Event t Word32)
  getAnySDLEvent :: m (Event t EventPayload)
  getWindowShownEvent :: m (Event t WindowShownEventData)
  getWindowHiddenEvent :: m (Event t WindowHiddenEventData)
  getWindowExposedEvent :: m (Event t WindowExposedEventData)
  getWindowMovedEvent :: m (Event t WindowMovedEventData)
  getWindowResizedEvent :: m (Event t WindowResizedEventData)
  getWindowSizeChangedEvent :: m (Event t WindowSizeChangedEventData)
  getWindowMinimizedEvent :: m (Event t WindowMinimizedEventData)
  getWindowMaximizedEvent :: m (Event t WindowMaximizedEventData)
  getWindowRestoredEvent :: m (Event t WindowRestoredEventData)
  getWindowGainedMouseFocusEvent :: m (Event t WindowGainedMouseFocusEventData)
  getWindowLostMouseFocusEvent :: m (Event t WindowLostMouseFocusEventData)
  getWindowGainedKeyboardFocusEvent :: m (Event t WindowGainedKeyboardFocusEventData)
  getWindowLostKeyboardFocusEvent :: m (Event t WindowLostKeyboardFocusEventData)
  getWindowClosedEvent :: m (Event t WindowClosedEventData)
  getKeyboardEvent :: m (Event t KeyboardEventData)
  getTextEditingEvent :: m (Event t TextEditingEventData)
  getTextInputEvent :: m (Event t TextInputEventData)
  getKeymapChangedEvent :: m (Event t ())
  getMouseMotionEvent :: m (Event t MouseMotionEventData)
  getMouseButtonEvent :: m (Event t MouseButtonEventData)
  getMouseWheelEvent :: m (Event t MouseWheelEventData)
  getJoyAxisEvent :: m (Event t JoyAxisEventData)
  getJoyBallEvent :: m (Event t JoyBallEventData)
  getJoyHatEvent :: m (Event t JoyHatEventData)
  getJoyButtonEvent :: m (Event t JoyButtonEventData)
  getJoyDeviceEvent :: m (Event t JoyDeviceEventData)
  getControllerAxisEvent :: m (Event t ControllerAxisEventData)
  getControllerButtonEvent :: m (Event t ControllerButtonEventData)
  getControllerDeviceEvent :: m (Event t ControllerDeviceEventData)
  getAudioDeviceEvent :: m (Event t AudioDeviceEventData)
  getQuitEvent :: m (Event t ())
  getUserEvent :: m (Event t UserEventData)
  getSysWMEvent :: m (Event t SysWMEventData)
  getTouchFingerEvent :: m (Event t TouchFingerEventData)
  getMultiGestureEvent :: m (Event t MultiGestureEventData)
  getDollarGestureEvent :: m (Event t DollarGestureEventData)
  getDropEvent :: m (Event t DropEventData)
  getClipboardUpdateEvent :: m (Event t ())
  getUnknownEvent :: m (Event t UnknownEventData)
  getQuitVar :: m (MVar ())


instance HasSDL2Events t m => HasSDL2Events t (ReaderT r m) where
  getTicksEvent = lift getTicksEvent
  getAnySDLEvent = lift getAnySDLEvent
  getWindowShownEvent = lift getWindowShownEvent
  getWindowHiddenEvent = lift getWindowHiddenEvent
  getWindowExposedEvent = lift getWindowExposedEvent
  getWindowMovedEvent = lift getWindowMovedEvent
  getWindowResizedEvent = lift getWindowResizedEvent
  getWindowSizeChangedEvent = lift getWindowSizeChangedEvent
  getWindowMinimizedEvent = lift getWindowMinimizedEvent
  getWindowMaximizedEvent = lift getWindowMaximizedEvent
  getWindowRestoredEvent = lift getWindowRestoredEvent
  getWindowGainedMouseFocusEvent = lift getWindowGainedMouseFocusEvent
  getWindowLostMouseFocusEvent = lift getWindowLostMouseFocusEvent
  getWindowGainedKeyboardFocusEvent = lift getWindowGainedKeyboardFocusEvent
  getWindowLostKeyboardFocusEvent = lift getWindowLostKeyboardFocusEvent
  getWindowClosedEvent = lift getWindowClosedEvent
  getKeyboardEvent = lift getKeyboardEvent
  getTextEditingEvent = lift getTextEditingEvent
  getTextInputEvent = lift getTextInputEvent
  getKeymapChangedEvent = lift getKeymapChangedEvent
  getMouseMotionEvent = lift getMouseMotionEvent
  getMouseButtonEvent = lift getMouseButtonEvent
  getMouseWheelEvent = lift getMouseWheelEvent
  getJoyAxisEvent = lift getJoyAxisEvent
  getJoyBallEvent = lift getJoyBallEvent
  getJoyHatEvent = lift getJoyHatEvent
  getJoyButtonEvent = lift getJoyButtonEvent
  getJoyDeviceEvent = lift getJoyDeviceEvent
  getControllerAxisEvent = lift getControllerAxisEvent
  getControllerButtonEvent = lift getControllerButtonEvent
  getControllerDeviceEvent = lift getControllerDeviceEvent
  getAudioDeviceEvent = lift getAudioDeviceEvent
  getQuitEvent = lift getQuitEvent
  getUserEvent = lift getUserEvent
  getSysWMEvent = lift getSysWMEvent
  getTouchFingerEvent = lift getTouchFingerEvent
  getMultiGestureEvent = lift getMultiGestureEvent
  getDollarGestureEvent = lift getDollarGestureEvent
  getDropEvent = lift getDropEvent
  getClipboardUpdateEvent = lift getClipboardUpdateEvent
  getUnknownEvent = lift getUnknownEvent
  getQuitVar = lift getQuitVar


instance HasSDL2Events t m => HasSDL2Events t (DynamicWriterT t w m) where
  getTicksEvent = lift getTicksEvent
  getAnySDLEvent = lift getAnySDLEvent
  getWindowShownEvent = lift getWindowShownEvent
  getWindowHiddenEvent = lift getWindowHiddenEvent
  getWindowExposedEvent = lift getWindowExposedEvent
  getWindowMovedEvent = lift getWindowMovedEvent
  getWindowResizedEvent = lift getWindowResizedEvent
  getWindowSizeChangedEvent = lift getWindowSizeChangedEvent
  getWindowMinimizedEvent = lift getWindowMinimizedEvent
  getWindowMaximizedEvent = lift getWindowMaximizedEvent
  getWindowRestoredEvent = lift getWindowRestoredEvent
  getWindowGainedMouseFocusEvent = lift getWindowGainedMouseFocusEvent
  getWindowLostMouseFocusEvent = lift getWindowLostMouseFocusEvent
  getWindowGainedKeyboardFocusEvent = lift getWindowGainedKeyboardFocusEvent
  getWindowLostKeyboardFocusEvent = lift getWindowLostKeyboardFocusEvent
  getWindowClosedEvent = lift getWindowClosedEvent
  getKeyboardEvent = lift getKeyboardEvent
  getTextEditingEvent = lift getTextEditingEvent
  getTextInputEvent = lift getTextInputEvent
  getKeymapChangedEvent = lift getKeymapChangedEvent
  getMouseMotionEvent = lift getMouseMotionEvent
  getMouseButtonEvent = lift getMouseButtonEvent
  getMouseWheelEvent = lift getMouseWheelEvent
  getJoyAxisEvent = lift getJoyAxisEvent
  getJoyBallEvent = lift getJoyBallEvent
  getJoyHatEvent = lift getJoyHatEvent
  getJoyButtonEvent = lift getJoyButtonEvent
  getJoyDeviceEvent = lift getJoyDeviceEvent
  getControllerAxisEvent = lift getControllerAxisEvent
  getControllerButtonEvent = lift getControllerButtonEvent
  getControllerDeviceEvent = lift getControllerDeviceEvent
  getAudioDeviceEvent = lift getAudioDeviceEvent
  getQuitEvent = lift getQuitEvent
  getUserEvent = lift getUserEvent
  getSysWMEvent = lift getSysWMEvent
  getTouchFingerEvent = lift getTouchFingerEvent
  getMultiGestureEvent = lift getMultiGestureEvent
  getDollarGestureEvent = lift getDollarGestureEvent
  getDropEvent = lift getDropEvent
  getClipboardUpdateEvent = lift getClipboardUpdateEvent
  getUnknownEvent = lift getUnknownEvent
  getQuitVar = lift getQuitVar
