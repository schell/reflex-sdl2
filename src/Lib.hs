{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Lib where

import           Control.Monad            (void)
import           Control.Monad.Fix        (MonadFix)
import           Control.Monad.Identity   (Identity (..))
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.Reader     (MonadReader, ReaderT (..), asks,
                                           runReaderT)
import           Control.Monad.Ref
import           Data.Dependent.Sum       (DSum ((:=>)))
import           Data.Function            (fix)
import           Data.IORef               (readIORef)
import           GHC.TypeLits             (KnownSymbol)
import           Graphics.Glucose
import           Graphics.Glucose.OpenGL
import           Graphics.Gristle         (Binding (..), GLContext (..),
                                           GLFragName, HasContext (..),
                                           IxShader)
import           Reflex
import           Reflex.Class
import           Reflex.Host.Class
import           Reflex.PerformEvent.Base
import           SDL                      hiding (Event, GLContext, OpenGL,
                                           glBindTexture)
import           System.Exit              (exitFailure)

import           Shaders
import           Shaders.Utils


ixPassthru :: IO (Either String (GLProgram (OpenGL IO)))
ixPassthru = compileIxProgram @'OpenGLContext @(OpenGL IO) @IO
                              opengl passthruVertex passthruFragment


data SystemEvents t = SystemEvents { sysEventPostBuild :: Event t ()
                                   , sysEventSDL       :: Event t EventPayload
                                   }


host
  :: Window
  -> ReaderT (SystemEvents Spider) (PerformEventT Spider (SpiderHost Global)) a
  -> IO ()
host window app = runSpiderHost $ do
  -- Get events and trigger refs for all things that can happen.
  (evSDL, trSDLRef)             <- newEventWithTriggerRef
  (evPostBuild, trPostBuildRef) <- newEventWithTriggerRef
  -- Build the network and get our firing command to trigger the post build event.
  (_, FireCommand fire) <-
    hostPerformEventT $ runReaderT app SystemEvents{ sysEventSDL       = evSDL
                                                   , sysEventPostBuild = evPostBuild
                                                   }
  -- Trigger the post build event.
  (readRef trPostBuildRef >>=) . mapM $ \postBuildTrigger ->
    fire [postBuildTrigger :=> Identity ()] $ return ()

  -- Loop forever getting sdl2 events and triggering them.
  fix $ \loop -> do
    payload <- eventPayload <$> waitEvent
    liftIO (readIORef trSDLRef) >>= \case
      Nothing       -> return ()
      Just trigger -> fireEvents [trigger :=> Identity payload]

    liftIO $ mySwapWindow window
    loop


putDebugLnE
  :: (PerformEvent t m, Reflex t, MonadIO (Performable m))
  => Event t a
  -> (a -> String)
  -> m ()
putDebugLnE ev showf = performEvent_ $ liftIO . putStrLn . showf <$> ev


--sz <- glGetDrawableSize window
--let V2 w h = fromIntegral <$> sz
--    t      = fromIntegral millis / 1000
--glUniform1f uTime t
--glUniform2f uResolution w h
--glClear $ fromIntegral gl_COLOR_BUFFER_BIT
--glViewport 0 0 (fromIntegral $ floor w) (fromIntegral $ floor h)
--glBindVertexArray vao
--glDrawArrays gl_TRIANGLE_FAN 0 4


mySwapWindow
  :: Window -> IO ()
mySwapWindow window = do
  let GLES{..} = gles $ opengl @IO
  glClear $ fromIntegral gl_COLOR_BUFFER_BIT
  glSwapWindow window


swapWindowOn
  :: forall t m a.
     ( PerformEvent t m
     , Reflex t
     , MonadIO (Performable m)
     )
  => Window
  -> Event t a
  -> m ()
swapWindowOn window ev =
  performEvent_ $ ffor ev $ const $ liftIO $ mySwapWindow window


guest
  :: forall m t.
     ( Reflex t
     , MonadHold t m
     , PerformEvent t m
     , MonadFix m
     , MonadIO m
     , MonadIO (Performable m)
     , MonadReader (SystemEvents t) m
     )
  => Window -> m ()
guest window = do
  evPostBuild <- asks sysEventPostBuild
  evSDL       <- asks sysEventSDL
  putDebugLnE evPostBuild show 
  putDebugLnE evSDL show
  return ()


someFunc :: IO ()
someFunc = do
  putStrLn "starting up..."
  initializeAll
  let ogl = defaultOpenGL{ glProfile = Core Debug 3 3 }
      cfg = defaultWindow{ windowOpenGL      = Just ogl
                         , windowResizable   = True
                         , windowHighDPI     = False
                         , windowInitialSize = V2 640 480
                         }
  window <- createWindow "Reflex GLish" cfg
  void $ glCreateContext window
  host window $ guest window
