{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main where

import           Control.Monad (forM_, void)
import           Reflex.SDL2
import           System.Exit   (exitSuccess)


data AABB = AABB InputMotion (V2 Int)


mouseButtonToAABB :: MouseButtonEventData -> AABB
mouseButtonToAABB dat = AABB (mouseButtonEventMotion dat) pos
  where P pos32 = mouseButtonEventPos dat
        pos = fromIntegral <$> pos32


motionToColor :: InputMotion -> V4 Int
motionToColor Released = V4 255 255 0   128
motionToColor Pressed  = V4 255 0   255 128


renderAABBs :: MonadIO m => Maybe Renderer -> [AABB] -> Maybe (m ())
renderAABBs mayRenderer aabbs = ffor mayRenderer $ \r -> do
  rendererDrawColor r $= V4 0 0 0 255
  clear r
  forM_ aabbs $ \(AABB motion pos) -> do
    let color = motionToColor motion
    rendererDrawColor r $= (fromIntegral <$> color)
    fillRect r $ Just $ Rectangle (P $ fromIntegral <$> pos - 10) 20
  present r


guest :: ReflexSDL2 t m => m ()
guest = do
  -- Print some stuff after the network is built.
  evPB <- asks sysPostBuildEvent
  performEvent_ $ ffor evPB $ \() ->
    liftIO $ putStrLn "starting up..."

  -- Get the window shown event and use it to create a renderer event.
  -- We use 'headE' because we only want to create one renderer, ever.
  evWindow   <- headE =<< asks (fmap windowShownEventWindow . sysWindowShownEvent)
  evRenderer <- performEvent $ ffor evWindow $ \window -> do
    liftIO $ putStrLn "creating renderer..."
    r <- createRenderer window (-1) defaultRenderer
    rendererDrawBlendMode r $= BlendAdditive
    return r

  -- Get any mouse button event and accumulate them as a list of
  -- AABBs. Then combine the dynamic list of AABBs with the renderer
  -- to perform a rendering event.
  evMouseButton <- asks sysMouseButtonEvent
  dAABBs        <- foldDyn (:) [] $ mouseButtonToAABB <$> evMouseButton
  dMayRenderer  <- holdDyn Nothing $ Just <$> evRenderer
  let dRendering  = renderAABBs <$> dMayRenderer <*> dAABBs
      evRendering = fmapMaybe id $ updated dRendering
  performEvent_ evRendering

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
  createWindow "reflex-sdl2-exe" cfg >>= void . glCreateContext
  host guest
