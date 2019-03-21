{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- | This module contains a minimum yet convenient API needed to get started
-- writing reflex apps with sdl2.
--
-- For an example see
-- [app/Main.hs](https://github.com/schell/reflex-sdl2/blob/master/app/Main.hs)
module Reflex.SDL2
  ( -- * Running an app
    host

    -- * Gracefully shutting down an app
  , shutdownOn

    -- * The reflex-sdl2 class
  , HasSDL2Events (..)
    -- * Base transformer
  , ReflexSDL2T
    -- * Common constraints (most powerful but convenient)
  , ReflexSDL2
    -- * Concrete stack
  , ConcreteReflexSDL2

    -- * Higher order switching
  , holdView
  , dynView

    -- * Time and recurring timer events
  , TickInfo(..)
  , getDeltaTickEvent
  , performEventDelta

    -- * Async events
  , getAsyncEvent

    -- * Debugging
  , putDebugLnE

    -- * Re-exports
  , module SDL
  , MonadIO
  , liftIO
  ) where

import           Control.Concurrent       (newChan, newEmptyMVar, putMVar,
                                           readChan, takeMVar)
import           Control.Concurrent.Async (async, cancel)
import           Control.Monad            (forM_, unless, void)
import           Control.Monad.Fix        (MonadFix)
import           Control.Monad.Identity   (Identity (..))
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.Reader
import           Control.Monad.Ref        (readRef)
import           Data.Dependent.Sum       (DSum ((:=>)))
import           Data.Function            (fix)
import           Data.Word                (Word32)
import           GHC.Conc                 (atomically, newTVar, readTVar,
                                           readTVarIO, writeTVar)
import           Reflex                   hiding (Additive)
import           Reflex.Host.Class
import           SDL                      hiding (Event, delay)

import           Reflex.SDL2.Base
import           Reflex.SDL2.Class
import           Reflex.SDL2.Internal


------------------------------------------------------------------------------
-- | A collection of constraints that represent the default reflex-sdl2 network.
type ReflexSDL2 t m = ( Reflex t
                      , MonadHold t m
                      , MonadSample t m
                      , Adjustable t m
                      , PostBuild t m
                      , PerformEvent t m
                      , TriggerEvent t m
                      , MonadFix m
                      , MonadIO m
                      , MonadIO (Performable m)
                      , HasSDL2Events t m
                      )


--------------------------------------------------------------------------------
-- | Returns an event that fires each frame with the number of milliseconds
-- since the last frame.
-- Be aware that subscribing to this 'Event' (by using it in a monadic action)
-- will result in your app running sdl2's event loop every frame.
getDeltaTickEvent
  :: (MonadHold t m, MonadFix m, HasSDL2Events t m) => m (Event t Word32)
getDeltaTickEvent = do
  let f (lastTick, _) thisTick = (thisTick, thisTick - lastTick)
  evTickAndDel <- accum f (0, 0) =<< getTicksEvent
  return $ snd <$> evTickAndDel


-- | Populate the event value with the time in milliseconds since the last time
-- the event fired.
performEventDelta :: ReflexSDL2 t m => Event t a -> m (Event t Word32)
performEventDelta ev = do
  tnow <- ticks
  evTicks <- performEvent $ ticks <$ ev
  fmap fst <$> accum (\(_, prev) now -> (now - prev, now)) (0, tnow) evTicks


--------------------------------------------------------------------------------
getAsyncEvent :: ReflexSDL2 t m => IO a -> m (Event t a)
getAsyncEvent f = do
  (ev, g) <- newTriggerEvent
  void $ liftIO $ async $ f >>= g
  return ev


--------------------------------------------------------------------------------
-- $grace
-- | Will exit the main reflex-sdl2 loop when the given Event fires. This allows
-- the programmer to shut down the network before shutting down SDL.
shutdownOn
  :: (PerformEvent t m, MonadIO (Performable m), HasSDL2Events t m)
  => Event t ()
  -> m ()
shutdownOn ev = do
  var <- getQuitVar
  performEvent_ $ liftIO (putMVar var ()) <$ ev


--------------------------------------------------------------------------------
-- | The monomorphic type used to run reflex-sdl2 apps.
type ConcreteReflexSDL2 = ReflexSDL2T Spider (TriggerEventT Spider (PostBuildT Spider (PerformEventT Spider (SpiderHost Global))))


------------------------------------------------------------------------------
-- | Host a reflex-sdl2 app.
host
  :: ConcreteReflexSDL2 ()
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
  (sysTouchFingerMotionEvent,                 trTouchFingerMotionRef) <- newEventWithTriggerRef
  (sysMultiGestureEvent,                           trMultiGestureRef) <- newEventWithTriggerRef
  (sysDollarGestureEvent,                         trDollarGestureRef) <- newEventWithTriggerRef
  (sysDropEvent,                                           trDropRef) <- newEventWithTriggerRef
  (sysClipboardUpdateEvent,                     trClipboardUpdateRef) <- newEventWithTriggerRef
  (sysUnknownEvent,                                     trUnknownRef) <- newEventWithTriggerRef

  -- Build the network and get our firing command to trigger the post build event,
  -- then loop forever in another thread, dequeueing triggers from our chan and
  -- placing them into a TVar. Push a new user event into the SDL event queue that
  -- will set off a read of the TVar and the firing of the triggers within the
  -- main loop.
  -- Also - create som quit vars to communicate when our loops should absolutely end.
  chan        <- liftIO newChan
  triggersVar <- liftIO $ atomically $ newTVar []
  sysQuitVar  <- liftIO newEmptyMVar
  mainQuitVar <- liftIO $ atomically $ newTVar False
  let reservedTriggerCode = 31337
      isJustTriggerData dat _ =
        return $ guard $ registeredEventCode dat == reservedTriggerCode
      fromData () = return emptyRegisteredEvent{ registeredEventCode  = reservedTriggerCode }
  pushTrig <- registerEvent isJustTriggerData fromData >>= \case
    Nothing -> error "Could not register an sdl event for TriggerEvent."
    Just (RegisteredEventType pushTrig _) -> return pushTrig
  asyncTrigger <- liftIO $ async $ fix $ \loop -> do
    trigs <- readChan chan
    atomically $ do
      prevTrigs <- readTVar triggersVar
      writeTVar triggersVar $ prevTrigs ++ trigs
    pushTrig () >>= \case
      EventPushSuccess   -> return ()
      EventPushFiltered  -> putStrLn "trigger push filtered"
      EventPushFailure t -> print t
    loop
  void $ liftIO $ async $ do
    takeMVar sysQuitVar
    atomically $ writeTVar mainQuitVar True
    void $ pushTrig ()
    cancel asyncTrigger

  ((), FireCommand fire) <-
    hostPerformEventT $ flip runPostBuildT sysPostBuildEvent
                      $ flip runTriggerEventT chan
                      $ runReflexSDL2T app SystemEvents{..}

  -- Trigger the post build event.
  (readRef trPostBuildRef >>=) . mapM_ $ \tr ->
    fire [tr :=> Identity ()] $ return ()

  -- Loop forever doing all of our main loop stuff.
  fix $ \loop -> do
    -- Fire any tick events if anyone is listening.
    -- If someone _is_ listening, we need to fire an
    -- event every frame - otherwise we can wait around
    -- for an sdl event to update the network.
    shouldWait <- readRef trTicksRef >>= \case
      Nothing -> return True
      Just tr -> do
        t <- ticks
        void $ fire [tr :=> Identity t] $ return ()
        return False

    payloads <- map eventPayload <$> if shouldWait
                                     then (:) <$> waitEvent
                                              <*> pollEvents
                                     else pollEvents

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
      UserEvent dat ->
        -- We've found some triggered reflex events, read them and fire them.
        if userEventCode dat == reservedTriggerCode
        then do
          triggers <- liftIO $ atomically $ do
            trigs <- readTVar triggersVar
            writeTVar triggersVar []
            return trigs
          forM_ triggers $ \(EventTriggerRef ref :=> TriggerInvocation a _cb) ->
            (readRef ref >>=) . mapM_ $ \tr -> fire [tr :=> Identity a] $ return ()
          -- Run the callbacks of those triggered events.
          forM_ triggers $ \(_ :=> TriggerInvocation _a cb) -> liftIO cb
        else (readRef trUserRef >>=) . mapM_ $ \tr -> fire [tr :=> Identity dat] $ return ()
      SysWMEvent dat -> (readRef trSysWMRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      TouchFingerEvent dat -> (readRef trTouchFingerRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity dat] $ return ()
      TouchFingerMotionEvent dat -> (readRef trTouchFingerMotionRef >>=) . mapM_ $ \tr ->
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

    shouldQuit <- liftIO $ readTVarIO mainQuitVar
    unless shouldQuit loop


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
-- | Run a placeholder network until the given 'Event' fires, then replace it
-- with the network of the 'Event's value. This process is repeated each time
-- the 'Event' fires a new network. Returns a 'Dynamic' of the inner network's
-- result that updates any time the 'Event' fires.
holdView :: ReflexSDL2 t m => m a -> Event t (m a) -> m (Dynamic t a)
holdView child0 newChild = do
  (result0, newResult) <- runWithReplace child0 newChild
  holdDyn result0 newResult


------------------------------------------------------------------------------
-- | Run a 'Dynamic'ally changing network, replacing the current one with the
-- new one every time the 'Dynamic' updates. Returns an 'Event' of the inner
-- network's result value that fires every time the 'Dynamic' changes.
dynView :: ReflexSDL2 t m => Dynamic t (m a) -> m (Event t a)
dynView child = do
  evPB <- getPostBuild
  let newChild = leftmost [updated child, tagCheap (current child) evPB]
  snd <$> runWithReplace (return ()) newChild
