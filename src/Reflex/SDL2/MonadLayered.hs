{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
-- | This module holds a convenient monad to write your application in. It
-- assumes a layered approach to rendering your screen. Your application
-- should write `Event t Layer` or somesuch yadda yadda.
module Reflex.SDL2.MonadLayered where

import           Reflex.SDL2
import Data.Semigroup (Semigroup)


type MonadLayered t w m = (ReflexSDL2 t m, EventWriter t w m)


runLayeredT :: (ReflexSDL2 t m, Semigroup w) => (w -> Performable m ()) -> EventWriterT t w m a -> m a
runLayeredT f app = do
  (a, evLayers) <- runEventWriterT app
  performEvent_ $ f <$> evLayers
  return a
