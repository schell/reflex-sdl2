{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- | This module contains the bare minimum needed to get started writing
-- reflex apps using sdl2.
--
-- For a tutorial see
-- [app/Main.hs](https://github.com/reflex-frp/reflex-sdl2/blob/master/app/Main.hs)
module Reflex.SDL2
  ( -- * All SDL events, packaged into reflex events
    SystemEvents(..)
    -- * Running an app
  , host
    -- * Debugging
  , putDebugLnE
    -- * Convenience constraints
  , ReflexSDL2
    -- * Re-exports
  , module Reflex
  , module SDL
  , MonadReader
  , asks
  , MonadIO
  , liftIO
  ) where

import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.Identity (Identity (..))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT (..), asks,
                                         runReaderT)
import           Control.Monad.Ref
import           Data.Dependent.Sum     (DSum ((:=>)))
import           Data.Function          (fix)
import           Data.Word              (Word32)
import           Reflex                 hiding (Additive)
import           Reflex.Host.Class
import           SDL                    hiding (Event)

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
  , sysMultiGestureEvent              :: Event t MultiGestureEventData
  , sysDollarGestureEvent             :: Event t DollarGestureEventData
  , sysDropEvent                      :: Event t DropEventData
  , sysClipboardUpdateEvent           :: Event t ()
  , sysUnknownEvent                   :: Event t UnknownEventData
  }


------------------------------------------------------------------------------
-- | Host a reflex-sdl2 app.
host
  :: ReaderT (SystemEvents Spider) (PerformEventT Spider (SpiderHost Global)) a
  -- ^ A concrete reflex-sdl2 network to run.
  -> IO ()
host app = runSpiderHost $ do
  -- Get events and trigger refs for all things that can happen.
  (evPostBuild,                                      trPostBuildRef) <- newEventWithTriggerRef
  (evAnySDL,                                            trAnySDLRef) <- newEventWithTriggerRef
  (evTicks,                                              trTicksRef) <- newEventWithTriggerRef
  (evWindowShownEvent,                             trWindowShownRef) <- newEventWithTriggerRef
  (evWindowHiddenEvent,                           trWindowHiddenRef) <- newEventWithTriggerRef
  (evWindowExposedEvent,                         trWindowExposedRef) <- newEventWithTriggerRef
  (evWindowMovedEvent,                             trWindowMovedRef) <- newEventWithTriggerRef
  (evWindowResizedEvent,                         trWindowResizedRef) <- newEventWithTriggerRef
  (evWindowSizeChangedEvent,                 trWindowSizeChangedRef) <- newEventWithTriggerRef
  (evWindowMinimizedEvent,                     trWindowMinimizedRef) <- newEventWithTriggerRef
  (evWindowMaximizedEvent,                     trWindowMaximizedRef) <- newEventWithTriggerRef
  (evWindowRestoredEvent,                       trWindowRestoredRef) <- newEventWithTriggerRef
  (evWindowGainedMouseFocusEvent,       trWindowGainedMouseFocusRef) <- newEventWithTriggerRef
  (evWindowLostMouseFocusEvent,           trWindowLostMouseFocusRef) <- newEventWithTriggerRef
  (evWindowGainedKeyboardFocusEvent, trWindowGainedKeyboardFocusRef) <- newEventWithTriggerRef
  (evWindowLostKeyboardFocusEvent,     trWindowLostKeyboardFocusRef) <- newEventWithTriggerRef
  (evWindowClosedEvent,                           trWindowClosedRef) <- newEventWithTriggerRef
  (evKeyboardEvent,                                   trKeyboardRef) <- newEventWithTriggerRef
  (evTextEditingEvent,                             trTextEditingRef) <- newEventWithTriggerRef
  (evTextInputEvent,                                 trTextInputRef) <- newEventWithTriggerRef
  (evKeymapChangedEvent,                         trKeymapChangedRef) <- newEventWithTriggerRef
  (evMouseMotionEvent,                             trMouseMotionRef) <- newEventWithTriggerRef
  (evMouseButtonEvent,                             trMouseButtonRef) <- newEventWithTriggerRef
  (evMouseWheelEvent,                               trMouseWheelRef) <- newEventWithTriggerRef
  (evJoyAxisEvent,                                     trJoyAxisRef) <- newEventWithTriggerRef
  (evJoyBallEvent,                                     trJoyBallRef) <- newEventWithTriggerRef
  (evJoyHatEvent,                                       trJoyHatRef) <- newEventWithTriggerRef
  (evJoyButtonEvent,                                 trJoyButtonRef) <- newEventWithTriggerRef
  (evJoyDeviceEvent,                                 trJoyDeviceRef) <- newEventWithTriggerRef
  (evControllerAxisEvent,                       trControllerAxisRef) <- newEventWithTriggerRef
  (evControllerButtonEvent,                   trControllerButtonRef) <- newEventWithTriggerRef
  (evControllerDeviceEvent,                   trControllerDeviceRef) <- newEventWithTriggerRef
  (evAudioDeviceEvent,                             trAudioDeviceRef) <- newEventWithTriggerRef
  (evQuitEvent,                                           trQuitRef) <- newEventWithTriggerRef
  (evUserEvent,                                           trUserRef) <- newEventWithTriggerRef
  (evSysWMEvent,                                         trSysWMRef) <- newEventWithTriggerRef
  (evTouchFingerEvent,                             trTouchFingerRef) <- newEventWithTriggerRef
  (evMultiGestureEvent,                           trMultiGestureRef) <- newEventWithTriggerRef
  (evDollarGestureEvent,                         trDollarGestureRef) <- newEventWithTriggerRef
  (evDropEvent,                                           trDropRef) <- newEventWithTriggerRef
  (evClipboardUpdateEvent,                     trClipboardUpdateRef) <- newEventWithTriggerRef
  (evUnknownEvent,                                     trUnknownRef) <- newEventWithTriggerRef

  -- Build the network and get our firing command to trigger the post build event.
  (_, FireCommand fire) <-
    hostPerformEventT $ runReaderT app
      SystemEvents{ sysPostBuildEvent                 = evPostBuild
                  , sysAnySDLEvent                    = evAnySDL
                  , sysTicksEvent                     = evTicks
                  , sysWindowShownEvent               = evWindowShownEvent
                  , sysWindowHiddenEvent              = evWindowHiddenEvent
                  , sysWindowExposedEvent             = evWindowExposedEvent
                  , sysWindowMovedEvent               = evWindowMovedEvent
                  , sysWindowResizedEvent             = evWindowResizedEvent
                  , sysWindowSizeChangedEvent         = evWindowSizeChangedEvent
                  , sysWindowMinimizedEvent           = evWindowMinimizedEvent
                  , sysWindowMaximizedEvent           = evWindowMaximizedEvent
                  , sysWindowRestoredEvent            = evWindowRestoredEvent
                  , sysWindowGainedMouseFocusEvent    = evWindowGainedMouseFocusEvent
                  , sysWindowLostMouseFocusEvent      = evWindowLostMouseFocusEvent
                  , sysWindowGainedKeyboardFocusEvent = evWindowGainedKeyboardFocusEvent
                  , sysWindowLostKeyboardFocusEvent   = evWindowLostKeyboardFocusEvent
                  , sysWindowClosedEvent              = evWindowClosedEvent
                  , sysKeyboardEvent                  = evKeyboardEvent
                  , sysTextEditingEvent               = evTextEditingEvent
                  , sysTextInputEvent                 = evTextInputEvent
                  , sysKeymapChangedEvent             = evKeymapChangedEvent
                  , sysMouseMotionEvent               = evMouseMotionEvent
                  , sysMouseButtonEvent               = evMouseButtonEvent
                  , sysMouseWheelEvent                = evMouseWheelEvent
                  , sysJoyAxisEvent                   = evJoyAxisEvent
                  , sysJoyBallEvent                   = evJoyBallEvent
                  , sysJoyHatEvent                    = evJoyHatEvent
                  , sysJoyButtonEvent                 = evJoyButtonEvent
                  , sysJoyDeviceEvent                 = evJoyDeviceEvent
                  , sysControllerAxisEvent            = evControllerAxisEvent
                  , sysControllerButtonEvent          = evControllerButtonEvent
                  , sysControllerDeviceEvent          = evControllerDeviceEvent
                  , sysAudioDeviceEvent               = evAudioDeviceEvent
                  , sysQuitEvent                      = evQuitEvent
                  , sysUserEvent                      = evUserEvent
                  , sysSysWMEvent                     = evSysWMEvent
                  , sysTouchFingerEvent               = evTouchFingerEvent
                  , sysMultiGestureEvent              = evMultiGestureEvent
                  , sysDollarGestureEvent             = evDollarGestureEvent
                  , sysDropEvent                      = evDropEvent
                  , sysClipboardUpdateEvent           = evClipboardUpdateEvent
                  , sysUnknownEvent                   = evUnknownEvent
                  }

  -- Trigger the post build event.
  (readRef trPostBuildRef >>=) . mapM_ $ \tr ->
    fire [tr :=> Identity ()] $ return ()

  ---- Loop forever getting sdl2 events and triggering them.
  fix $ \loop -> do
    payload <- eventPayload <$> waitEvent
    case payload of
      WindowShownEvent dat -> (readRef trWindowShownRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      WindowHiddenEvent dat -> (readRef trWindowHiddenRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      WindowExposedEvent dat -> (readRef trWindowExposedRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      WindowMovedEvent dat -> (readRef trWindowMovedRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      WindowResizedEvent dat -> (readRef trWindowResizedRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      WindowSizeChangedEvent dat -> (readRef trWindowSizeChangedRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      WindowMinimizedEvent dat -> (readRef trWindowMinimizedRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      WindowMaximizedEvent dat -> (readRef trWindowMaximizedRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      WindowRestoredEvent dat -> (readRef trWindowRestoredRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      WindowGainedMouseFocusEvent dat -> (readRef trWindowGainedMouseFocusRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      WindowLostMouseFocusEvent dat -> (readRef trWindowLostMouseFocusRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      WindowGainedKeyboardFocusEvent dat -> (readRef trWindowGainedKeyboardFocusRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      WindowLostKeyboardFocusEvent dat -> (readRef trWindowLostKeyboardFocusRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      WindowClosedEvent dat -> (readRef trWindowClosedRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      KeyboardEvent dat -> (readRef trKeyboardRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      TextEditingEvent dat -> (readRef trTextEditingRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      TextInputEvent dat -> (readRef trTextInputRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      KeymapChangedEvent -> (readRef trKeymapChangedRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity ()] $ return ()
      MouseMotionEvent dat -> (readRef trMouseMotionRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      MouseButtonEvent dat -> (readRef trMouseButtonRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      MouseWheelEvent dat -> (readRef trMouseWheelRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      JoyAxisEvent dat -> (readRef trJoyAxisRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      JoyBallEvent dat -> (readRef trJoyBallRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      JoyHatEvent dat -> (readRef trJoyHatRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      JoyButtonEvent dat -> (readRef trJoyButtonRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      JoyDeviceEvent dat -> (readRef trJoyDeviceRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      ControllerAxisEvent dat -> (readRef trControllerAxisRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      ControllerButtonEvent dat -> (readRef trControllerButtonRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      ControllerDeviceEvent dat -> (readRef trControllerDeviceRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      AudioDeviceEvent dat -> (readRef trAudioDeviceRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      QuitEvent -> (readRef trQuitRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity ()] $ return ()
      UserEvent dat -> (readRef trUserRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      SysWMEvent dat -> (readRef trSysWMRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      TouchFingerEvent dat -> (readRef trTouchFingerRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      MultiGestureEvent dat -> (readRef trMultiGestureRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      DollarGestureEvent dat -> (readRef trDollarGestureRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      DropEvent dat -> (readRef trDropRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      ClipboardUpdateEvent -> (readRef trClipboardUpdateRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity ()] $ return ()
      UnknownEvent dat -> (readRef trUnknownRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()

    -- Fire an event for the wrapped payload as well.
    (readRef trAnySDLRef >>=) . mapM_ $ \tr ->
      fire [tr :=> Identity payload] $ return ()
    -- Fire any tick events, if anyone is listening.
    (readRef trTicksRef >>=) . mapM_ $ \tr -> ticks >>= \t ->
      fire [tr :=> Identity t] $ return ()

    loop


------------------------------------------------------------------------------
-- | Like 'putStrLn', but for 'Event's.
putDebugLnE
  :: (PerformEvent t m, Reflex t, MonadIO (Performable m))
  => Event t a
  -- ^ The 'Event' to trigger the print.
  -> (a -> String)
  -- ^ A function to show the 'Event's value.
  -> m ()
putDebugLnE ev showf = performEvent_ $ liftIO . putStrLn . showf <$> ev


------------------------------------------------------------------------------
-- | A collection of constraints that represent a reflex-sdl2 network.
type ReflexSDL2 t m =
  ( Reflex t
  , MonadHold t m
  , PerformEvent t m
  , MonadFix m
  , MonadIO m
  , MonadIO (Performable m)
  , MonadReader (SystemEvents t) m
  )
