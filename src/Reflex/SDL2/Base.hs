{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Reflex.SDL2.Base
  ( ReflexSDL2T (..)
  , runReflexSDL2T
  ) where


import           Control.Monad.Exception  (MonadException)
import           Control.Monad.IO.Class   (MonadIO)
import           Control.Monad.Reader
import           Reflex                   hiding (Additive)
import           Reflex.Host.Class

import           Reflex.SDL2.Class
import           Reflex.SDL2.Internal


------------------------------------------------------------------------------
-- | Provides an implementation of the 'HasSDL2Events' type class.
newtype ReflexSDL2T t (m :: * -> *) a =
  ReflexSDL2T { unReflexSDL2T :: ReaderT (SystemEvents t) m a }


runReflexSDL2T :: ReflexSDL2T t m a -> SystemEvents t -> m a
runReflexSDL2T = runReaderT . unReflexSDL2T


deriving instance (ReflexHost t, Functor m)        => Functor (ReflexSDL2T t m)
deriving instance (ReflexHost t, Applicative m)    => Applicative (ReflexSDL2T t m)
deriving instance (ReflexHost t, Monad m)          => Monad (ReflexSDL2T t m)
deriving instance (ReflexHost t, MonadFix m)       => MonadFix (ReflexSDL2T t m)
deriving instance (ReflexHost t, MonadIO m)        => MonadIO (ReflexSDL2T t m)
deriving instance ReflexHost t                     => MonadTrans (ReflexSDL2T t)
deriving instance (ReflexHost t, MonadException m) => MonadException (ReflexSDL2T t m)
deriving instance (ReflexHost t, TriggerEvent t m) => TriggerEvent t (ReflexSDL2T t m)


askSys :: Monad m => (SystemEvents t -> a) -> ReflexSDL2T t m a
askSys = ReflexSDL2T . asks


instance (ReflexHost t, Monad m) => HasSDL2Events t (ReflexSDL2T t m) where
  getTicksEvent = askSys sysTicksEvent
  getAnySDLEvent = askSys sysAnySDLEvent
  getWindowShownEvent = askSys sysWindowShownEvent
  getWindowHiddenEvent = askSys sysWindowHiddenEvent
  getWindowExposedEvent = askSys sysWindowExposedEvent
  getWindowMovedEvent = askSys sysWindowMovedEvent
  getWindowResizedEvent = askSys sysWindowResizedEvent
  getWindowSizeChangedEvent = askSys sysWindowSizeChangedEvent
  getWindowMinimizedEvent = askSys sysWindowMinimizedEvent
  getWindowMaximizedEvent = askSys sysWindowMaximizedEvent
  getWindowRestoredEvent = askSys sysWindowRestoredEvent
  getWindowGainedMouseFocusEvent = askSys sysWindowGainedMouseFocusEvent
  getWindowLostMouseFocusEvent = askSys sysWindowLostMouseFocusEvent
  getWindowGainedKeyboardFocusEvent = askSys sysWindowGainedKeyboardFocusEvent
  getWindowLostKeyboardFocusEvent = askSys sysWindowLostKeyboardFocusEvent
  getWindowClosedEvent = askSys sysWindowClosedEvent
  getKeyboardEvent = askSys sysKeyboardEvent
  getTextEditingEvent = askSys sysTextEditingEvent
  getTextInputEvent = askSys sysTextInputEvent
  getKeymapChangedEvent = askSys sysKeymapChangedEvent
  getMouseMotionEvent = askSys sysMouseMotionEvent
  getMouseButtonEvent = askSys sysMouseButtonEvent
  getMouseWheelEvent = askSys sysMouseWheelEvent
  getJoyAxisEvent = askSys sysJoyAxisEvent
  getJoyBallEvent = askSys sysJoyBallEvent
  getJoyHatEvent = askSys sysJoyHatEvent
  getJoyButtonEvent = askSys sysJoyButtonEvent
  getJoyDeviceEvent = askSys sysJoyDeviceEvent
  getControllerAxisEvent = askSys sysControllerAxisEvent
  getControllerButtonEvent = askSys sysControllerButtonEvent
  getControllerDeviceEvent = askSys sysControllerDeviceEvent
  getAudioDeviceEvent = askSys sysAudioDeviceEvent
  getQuitEvent = askSys sysQuitEvent
  getUserEvent = askSys sysUserEvent
  getSysWMEvent = askSys sysSysWMEvent
  getTouchFingerEvent = askSys sysTouchFingerEvent
  getMultiGestureEvent = askSys sysMultiGestureEvent
  getDollarGestureEvent = askSys sysDollarGestureEvent
  getDropEvent = askSys sysDropEvent
  getClipboardUpdateEvent = askSys sysClipboardUpdateEvent
  getUnknownEvent = askSys sysUnknownEvent
  getQuitVar = askSys sysQuitVar


------------------------------------------------------------------------------
-- | 'ReflexSDL2T' is an instance of 'PostBuild'.
instance (Reflex t, PostBuild t m, ReflexHost t, Monad m) => PostBuild t (ReflexSDL2T t m) where
  getPostBuild = lift getPostBuild


------------------------------------------------------------------------------
-- | 'ReflexSDL2T' is an instance of 'PerformEvent'.
instance (ReflexHost t, PerformEvent t m) => PerformEvent t (ReflexSDL2T t m) where
  type Performable (ReflexSDL2T t m) = ReflexSDL2T t (Performable m)
  performEvent_ = ReflexSDL2T . performEvent_ . fmap unReflexSDL2T
  performEvent  = ReflexSDL2T . performEvent  . fmap unReflexSDL2T


------------------------------------------------------------------------------
-- | 'ReflexSDL2T' is an instance of 'Adjustable'.
instance ( Reflex t
         , ReflexHost t
         , Adjustable t m
         , Monad m
         ) => Adjustable t (ReflexSDL2T t m) where
  runWithReplace ma evmb =
    ReflexSDL2T $ runWithReplace (unReflexSDL2T ma) (unReflexSDL2T <$> evmb)
  traverseDMapWithKeyWithAdjust kvma dMapKV = ReflexSDL2T .
    traverseDMapWithKeyWithAdjust (\ka -> unReflexSDL2T . kvma ka) dMapKV
  traverseDMapWithKeyWithAdjustWithMove kvma dMapKV = ReflexSDL2T .
    traverseDMapWithKeyWithAdjustWithMove (\ka -> unReflexSDL2T . kvma ka) dMapKV
  traverseIntMapWithKeyWithAdjust f im = ReflexSDL2T .
    traverseIntMapWithKeyWithAdjust (\ka -> unReflexSDL2T . f ka) im


------------------------------------------------------------------------------
-- | 'ReflexSDL2T' is an instance of 'MonadHold'.
instance ( ReflexHost t
         , Applicative m
         , Monad m
         , MonadSample t m
         ) => MonadSample t (ReflexSDL2T t m) where
  sample = lift . sample


------------------------------------------------------------------------------
-- | 'ReflexSDL2T' is an instance of 'MonadHold'.
instance (ReflexHost t, MonadHold t m) => MonadHold t (ReflexSDL2T t m) where
  hold a = lift . hold a
  holdDyn a = lift . holdDyn a
  holdIncremental p = lift . holdIncremental p
  buildDynamic ma = lift . buildDynamic ma
  headE = lift . headE
