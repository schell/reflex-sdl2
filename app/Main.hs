{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main where

import           Control.Monad (forM_, void)
import           Reflex.SDL2
import           Reflex.SDL2.MonadLayered
import           System.Exit   (exitSuccess)


data AABB = AABB InputMotion (V2 Int)


mouseButtonToAABB :: MouseButtonEventData -> AABB
mouseButtonToAABB dat = AABB (mouseButtonEventMotion dat) pos
  where P pos32 = mouseButtonEventPos dat
        pos = fromIntegral <$> pos32


motionToColor :: InputMotion -> V4 Int
motionToColor Released = V4 255 255 0   128
motionToColor Pressed  = V4 255 0   255 128


renderAABB :: MonadIO m => Renderer -> V4 Int -> V2 Int -> m ()
renderAABB r color pos = do
  rendererDrawColor r $= (fromIntegral <$> color)
  fillRect r $ Just $ Rectangle (P $ fromIntegral <$> pos - 10) 20


guest :: MonadLayered t [Performable m ()] m => Renderer -> m ()
guest r = do
  -- Print some stuff after the network is built.
  evPB <- asks sysPostBuildEvent
  performEvent_ $ ffor evPB $ \() ->
    liftIO $ putStrLn "starting up..."

  -- Get any mouse button event and accumulate them as a list of
  -- AABBs. Then combine the dynamic list of AABBs with the renderer
  -- to perform a rendering event.
  evMouseButton <- asks sysMouseButtonEvent
  tellEvent $ ffor evMouseButton $ \dat -> pure $ do
    let AABB motion pos = mouseButtonToAABB dat
        color = motionToColor motion
    renderAABB r color pos

  evMouseMove <- asks sysMouseMotionEvent
  dMoves      <- foldDyn (\x xs -> take 100 $ x : xs) [] evMouseMove
  tellEvent $ ffor (updated dMoves) $ map $ \dat -> do
    let P pos = fromIntegral <$> mouseMotionEventPos dat
        color = V4 255 255 255 255
    renderAABB r color pos

  -- Quit on any quit event.
  evQuit <- asks sysQuitEvent
  performEvent_ $ ffor evQuit $ \() -> liftIO $ do
    putStrLn "bye!"
    exitSuccess

main :: IO ()
main = do
  initializeAll
  let ogl = defaultOpenGL{ glProfile = Core Debug 3 3 }
      cfg = defaultWindow{ windowOpenGL      = Just ogl
                         , windowResizable   = True
                         , windowHighDPI     = False
                         , windowInitialSize = V2 640 480
                         }
  window <- createWindow "reflex-sdl2-exe" cfg
  void $ glCreateContext window

  putStrLn "creating renderer..."
  r <- createRenderer window (-1) defaultRenderer
  rendererDrawBlendMode r $= BlendAdditive

  host $ do
    (_, evLayers) <- runEventWriterT $ guest r
    performEvent_ $ ffor evLayers $ \layers -> do
      rendererDrawColor r $= V4 0 0 0 255
      clear r
      sequence_ layers
      present r
