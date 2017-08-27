{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
-- | This module contains the bare minimum needed to get started writing
-- reflex apps using sdl2.
--
-- For an example see
-- [app/Main.hs](https://github.com/reflex-frp/reflex-sdl2/blob/master/app/Main.hs)
module Reflex.SDL2
  ( -- * Convenience constraints
    ReflexSDL2
    -- * Running an app
  , host
    -- * All SDL events, packaged into reflex events
  , SystemEvents(..)
    -- * Higher order switching
  , holdView
  , dynView
    -- * Common Events and Dynamics
  , getDeltaTickEvent
    -- * Debugging
  , putDebugLnE
    -- * Re-exports
  , module Reflex
  , module SDL
  , MonadReader
  , asks
  , MonadIO
  , liftIO
  ) where

import           Control.Monad           (forM_, void)
import           Control.Monad.Exception (MonadException)
import           Control.Monad.Fix       (MonadFix)
import           Control.Monad.Identity  (Identity (..))
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Primitive (PrimMonad)
import           Control.Monad.Reader
import           Control.Monad.Ref       (Ref, readRef)
import           Data.Dependent.Sum      (DSum ((:=>)))
import           Data.Function           (fix)
import           Data.Word               (Word32)
import           Reflex                  hiding (Additive)
import           Reflex.Host.Class
import           SDL                     hiding (Event)

import           Reflex.SDL2.Internal    as RSDL2 hiding (sysPostBuildEvent)
import           Reflex.SDL2.Internal    (sysPostBuildEvent)
import           Reflex.Spider.Internal  (HasSpiderTimeline)


------------------------------------------------------------------------------
-- | A collection of constraints that represent the default reflex-sdl2 network.
type ReflexSDL2 t m =
  ( Reflex t
  , MonadHold t m
  , MonadSample t m
  , MonadAdjust t m
  , PostBuild t m
  , PerformEvent t m
  , MonadFix m
  , MonadIO m
  , MonadIO (Performable m)
  , MonadReader (SystemEvents t) m
  )


------------------------------------------------------------------------------
-- | Provides a basic implementation of 'ReflexSDL2 t m' constraints.
newtype ReflexSDL2T t (m :: * -> *) a =
  ReflexSDL2T { runReflexSDL2T :: ReaderT (SystemEvents t) (PerformEventT t m) a }

deriving instance ReflexHost t => Functor (ReflexSDL2T t m)
deriving instance ReflexHost t => Applicative (ReflexSDL2T t m)
deriving instance ReflexHost t => Monad (ReflexSDL2T t m)
deriving instance ReflexHost t => MonadFix (ReflexSDL2T t m)
deriving instance ReflexHost t => MonadReader (SystemEvents t) (ReflexSDL2T t m)
deriving instance (ReflexHost t, MonadIO (HostFrame t)) => MonadIO (ReflexSDL2T t m)
deriving instance (ReflexHost t, MonadException (HostFrame t)) => MonadException (ReflexSDL2T t m)


------------------------------------------------------------------------------
-- | 'ReflexSDL2T' is an instance of 'PostBuild'.
instance (Reflex t, ReflexHost t, Monad m) => PostBuild t (ReflexSDL2T t m) where
  getPostBuild = asks sysPostBuildEvent


------------------------------------------------------------------------------
-- | 'ReflexSDL2T' is an instance of 'PerformEvent'.
instance ( ReflexHost t
         , Ref m ~ Ref IO
         , PrimMonad (HostFrame t)
         ) => PerformEvent t (ReflexSDL2T t m) where
  type Performable (ReflexSDL2T t m) = HostFrame t
  performEvent_ = ReflexSDL2T . lift . performEvent_
  performEvent  = ReflexSDL2T . lift . performEvent


------------------------------------------------------------------------------
-- | 'ReflexSDL2T' is an instance of 'MonadAdjust'.
instance ( Reflex t
         , ReflexHost t
         , PrimMonad (HostFrame t)
         ) => MonadAdjust t (ReflexSDL2T t m) where
  runWithReplace ma evmb =
    ReflexSDL2T $ runWithReplace (runReflexSDL2T ma) (runReflexSDL2T <$> evmb)
  traverseDMapWithKeyWithAdjust kvma dMapKV = ReflexSDL2T .
    traverseDMapWithKeyWithAdjust (\ka -> runReflexSDL2T . kvma ka) dMapKV
  traverseDMapWithKeyWithAdjustWithMove kvma dMapKV = ReflexSDL2T .
    traverseDMapWithKeyWithAdjustWithMove (\ka -> runReflexSDL2T . kvma ka) dMapKV


------------------------------------------------------------------------------
-- | 'ReflexSDL2T' is an instance of 'MonadHold'.
instance ReflexHost t => MonadSample t (ReflexSDL2T t m) where
  sample = ReflexSDL2T . sample


------------------------------------------------------------------------------
-- | 'ReflexSDL2T' is an instance of 'MonadHold'.
instance (ReflexHost t, MonadHold t m) => MonadHold t (ReflexSDL2T t m) where
  hold a = ReflexSDL2T . lift . hold a
  holdDyn a = ReflexSDL2T . lift . holdDyn a
  holdIncremental p = ReflexSDL2T . lift . holdIncremental p
  buildDynamic ma = ReflexSDL2T . lift . buildDynamic ma


------------------------------------------------------------------------------
-- | Returns an event that fires each frame with the number of milliseconds
-- since the last frame.
-- Be aware that subscribing to this 'Event' (by using it in a monadic action)
-- will result in your app running sdl2's event loop every frame.
getDeltaTickEvent :: ReflexSDL2 t m => m (Event t Word32)
getDeltaTickEvent = do
  let f (lastTick, _) thisTick = (thisTick, thisTick - lastTick)
  evTickAndDel <- accum f (0, 0) =<< asks sysTicksEvent
  return $ snd <$> evTickAndDel


------------------------------------------------------------------------------
-- | Host a reflex-sdl2 app.
host
  :: ReflexSDL2T Spider (SpiderHost Global) a
  -- ^ A concrete reflex-sdl2 network to run.
  -> IO ()
host app = runSpiderHost $ do
  -- Get events and trigger refs for all things that can happen.
  (sysPostBuildEvent,                                 trPostBuildRef) <- newEventWithTriggerRef
  (sysAnySDLEvent,                                       trAnySDLRef) <- newEventWithTriggerRef
  (sysTicksEvent,                                         trTicksRef) <- newEventWithTriggerRef
  (sysWindowShownEvent,                             trWindowShownRef) <- newEventWithTriggerRef
  (sysWindowHiddenEvent,                           trWindowHiddenRef) <- newEventWithTriggerRef
  (sysWindowExposedEvent,                         trWindowExposedRef) <- newEventWithTriggerRef
  (sysWindowMovedEvent,                             trWindowMovedRef) <- newEventWithTriggerRef
  (sysWindowResizedEvent,                         trWindowResizedRef) <- newEventWithTriggerRef
  (sysWindowSizeChangedEvent,                 trWindowSizeChangedRef) <- newEventWithTriggerRef
  (sysWindowMinimizedEvent,                     trWindowMinimizedRef) <- newEventWithTriggerRef
  (sysWindowMaximizedEvent,                     trWindowMaximizedRef) <- newEventWithTriggerRef
  (sysWindowRestoredEvent,                       trWindowRestoredRef) <- newEventWithTriggerRef
  (sysWindowGainedMouseFocusEvent,       trWindowGainedMouseFocusRef) <- newEventWithTriggerRef
  (sysWindowLostMouseFocusEvent,           trWindowLostMouseFocusRef) <- newEventWithTriggerRef
  (sysWindowGainedKeyboardFocusEvent, trWindowGainedKeyboardFocusRef) <- newEventWithTriggerRef
  (sysWindowLostKeyboardFocusEvent,     trWindowLostKeyboardFocusRef) <- newEventWithTriggerRef
  (sysWindowClosedEvent,                           trWindowClosedRef) <- newEventWithTriggerRef
  (sysKeyboardEvent,                                   trKeyboardRef) <- newEventWithTriggerRef
  (sysTextEditingEvent,                             trTextEditingRef) <- newEventWithTriggerRef
  (sysTextInputEvent,                                 trTextInputRef) <- newEventWithTriggerRef
  (sysKeymapChangedEvent,                         trKeymapChangedRef) <- newEventWithTriggerRef
  (sysMouseMotionEvent,                             trMouseMotionRef) <- newEventWithTriggerRef
  (sysMouseButtonEvent,                             trMouseButtonRef) <- newEventWithTriggerRef
  (sysMouseWheelEvent,                               trMouseWheelRef) <- newEventWithTriggerRef
  (sysJoyAxisEvent,                                     trJoyAxisRef) <- newEventWithTriggerRef
  (sysJoyBallEvent,                                     trJoyBallRef) <- newEventWithTriggerRef
  (sysJoyHatEvent,                                       trJoyHatRef) <- newEventWithTriggerRef
  (sysJoyButtonEvent,                                 trJoyButtonRef) <- newEventWithTriggerRef
  (sysJoyDeviceEvent,                                 trJoyDeviceRef) <- newEventWithTriggerRef
  (sysControllerAxisEvent,                       trControllerAxisRef) <- newEventWithTriggerRef
  (sysControllerButtonEvent,                   trControllerButtonRef) <- newEventWithTriggerRef
  (sysControllerDeviceEvent,                   trControllerDeviceRef) <- newEventWithTriggerRef
  (sysAudioDeviceEvent,                             trAudioDeviceRef) <- newEventWithTriggerRef
  (sysQuitEvent,                                           trQuitRef) <- newEventWithTriggerRef
  (sysUserEvent,                                           trUserRef) <- newEventWithTriggerRef
  (sysSysWMEvent,                                         trSysWMRef) <- newEventWithTriggerRef
  (sysTouchFingerEvent,                             trTouchFingerRef) <- newEventWithTriggerRef
  (sysMultiGestureEvent,                           trMultiGestureRef) <- newEventWithTriggerRef
  (sysDollarGestureEvent,                         trDollarGestureRef) <- newEventWithTriggerRef
  (sysDropEvent,                                           trDropRef) <- newEventWithTriggerRef
  (sysClipboardUpdateEvent,                     trClipboardUpdateRef) <- newEventWithTriggerRef
  (sysUnknownEvent,                                     trUnknownRef) <- newEventWithTriggerRef

  -- Build the network and get our firing command to trigger the post build event.
  (_, FireCommand fire) <-
    hostPerformEventT $ runReaderT (runReflexSDL2T app) SystemEvents{..}

  -- Trigger the post build event.
  (readRef trPostBuildRef >>=) . mapM_ $ \tr ->
    fire [tr :=> Identity ()] $ return ()

  ---- Loop forever getting sdl2 events and triggering them.
  fix $ \loop -> do
    -- Fire any tick events, if anyone is listening.
    -- If someone _is_ listening, we need to fire an
    -- event every frame - otherwise we can wait around
    -- for an sdl event to update the network.
    shouldWait <- readRef trTicksRef >>= \case
      Nothing -> return True
      Just tr -> do
        t <- ticks
        void $ fire [tr :=> Identity t] $ return ()
        return False

    payloads <- if shouldWait
                then pure . eventPayload <$> waitEvent
                else map    eventPayload <$> pollEvents

    forM_ payloads $ \case
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
      forM_ payloads $ \payload ->
        fire [tr :=> Identity payload] $ return ()

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


holdView :: ReflexSDL2 t m => m a -> Event t (m a) -> m (Dynamic t a)
holdView child0 newChild = do
  (result0, newResult) <- runWithReplace child0 newChild
  holdDyn result0 newResult


dynView :: ReflexSDL2 t m => Dynamic t (m a) -> m (Event t a)
dynView child = do
  evPB <- getPostBuild
  let newChild = leftmost [updated child, tagCheap (current child) evPB]
  snd <$> runWithReplace (return ()) newChild
