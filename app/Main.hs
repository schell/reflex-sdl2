{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
module Main where

import           Control.Monad            (forM_, guard, void)
import           Data.IORef
import           Data.Word                (Word8)
import           Reflex.SDL2
import           System.Exit              (exitSuccess)


data AABB = AABB InputMotion (V2 Int)


mouseButtonToAABB :: MouseButtonEventData -> AABB
mouseButtonToAABB dat = AABB (mouseButtonEventMotion dat) pos
  where P pos32 = mouseButtonEventPos dat
        pos = fromIntegral <$> pos32


motionToColor :: InputMotion -> V4 Int
motionToColor Released = V4 255 0 0   128
motionToColor Pressed  = V4 0   0 255 128


renderAABB :: MonadIO m => Renderer -> V4 Int -> V2 Int -> m ()
renderAABB r color pos = do
  rendererDrawColor r $= (fromIntegral <$> color)
  fillRect r $ Just $ Rectangle (P $ fromIntegral <$> pos - 10) 20

type Layer m = Performable m ()

layer :: (ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m)
      => Dynamic t [Layer m] -> m ()
layer = tellDyn

staticLayer :: (ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m)
            => Dynamic t (Layer m) -> m ()
staticLayer = tellDyn . fmap pure

ffor2 a b f = zipDynWith f a b
ffor2up a b = ffor (zipDyn a b)

guest
  :: (ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m)
  => Renderer
  -> m ()
guest r = do
  -- Print some stuff after the network is built.
  evPB <- asks sysPostBuildEvent
  performEvent_ $ ffor evPB $ \() ->
    liftIO $ putStrLn "starting up..."

  ------------------------------------------------------------------------------
  -- Ghosty trail of squares
  ------------------------------------------------------------------------------
  -- Gather all mouse motion events into a list, then commit a layer that
  -- renders each move as a quarter alpha'd yello or cyan square.
  evMouseMove <- asks sysMouseMotionEvent
  dMoves      <- foldDyn (\x xs -> take 100 $ x : xs) [] evMouseMove
  staticLayer $ ffor dMoves $ \moves ->
    forM_ (reverse moves) $ \dat -> do
      let P pos = fromIntegral <$> mouseMotionEventPos dat
          color = if null (mouseMotionEventState dat)
                  then V4 255 255 0   128
                  else V4 0   255 255 128
      renderAABB r color pos

  ------------------------------------------------------------------------------
  -- Up and down squares
  ------------------------------------------------------------------------------
  -- Get any mouse button event and accumulate them as a list of
  -- AABBs. Commit a layer of those rendered up/down AABBs.
  evMouseButton <- asks sysMouseButtonEvent
  dBtns         <- foldDyn (\x xs -> take 100 $ x : xs) [] evMouseButton
  staticLayer $ ffor dBtns $ \btns -> do
    forM_ (reverse btns) $ \dat -> do
      let AABB motion pos = mouseButtonToAABB dat
          color = motionToColor motion
      renderAABB r color pos

  ------------------------------------------------------------------------------
  -- An ephemeral layer that only renders when a key is down, and only listens
  -- to the tick event while that key is down.
  ------------------------------------------------------------------------------
  evKey <- asks sysKeyboardEvent
  let evKeyNoRepeat = fmapMaybe (\k -> k <$ guard (not $ keyboardEventRepeat k)) evKey
  dPressed <- holdDyn False $ ((== Pressed) . keyboardEventKeyMotion) <$> evKeyNoRepeat
  void $ holdView (return ()) $ ffor (updated dPressed) $ \case
    False -> return ()
    True  -> do
      evDeltaTick <- getDeltaTickEvent
      dTimePressed <- foldDyn (+) 0 evDeltaTick
      staticLayer $ ffor dTimePressed $ \t -> do
        let wrap :: Integral b => Float -> b
            wrap x = if x > 255 then wrap (x - 255) else floor x
            rc    = wrap $ fromIntegral t/1000   * 255
            gc    = wrap $ fromIntegral t/2000 * 255
            bc    = wrap $ fromIntegral t/3000 * 255
            color :: V4 Int
            color = fromIntegral <$> V4 rc gc bc 255
        renderAABB r color 100

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
  rendererDrawBlendMode r $= BlendAlphaBlend
  host $ do
    (_, dynLayers) <- runDynamicWriterT $ guest r
    performEvent_ $ ffor (updated dynLayers) $ \layers -> do
      rendererDrawColor r $= V4 0 0 0 255
      clear r
      sequence_ layers
      present r
