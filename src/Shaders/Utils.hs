{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Shaders.Utils where

import           Control.Monad                (mapM_)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Trans          (lift)
import           Control.Monad.Trans.Either   (EitherT (..), runEitherT)
import           Data.Maybe                   (catMaybes)

import           Graphics.Glucose
import           Graphics.Glucose.Utils       (compileProgram, compileShader)

import           Graphics.Gristle             (Binding (..), GLContext (..),
                                               HasContext (..), IxShader,
                                               getCtx, onlySrc)

------------------------------------------------------------------------------
-- IxShader helpers
------------------------------------------------------------------------------
compileIxShader
  :: forall (ctx :: GLContext) gl m j x. (HasContext ctx, MonadIO m, IsGLES m gl)
  => gl
  -> IxShader ctx '[] j x
  -> GLEnum gl 
  -> m (Either String (GLShader gl))
compileIxShader gl ixshader shadertype = runEitherT $ do
  let src0 = onlySrc ixshader
      src1 = case getCtx @ctx of
        OpenGLContext -> "#version 330 core\n" ++ src0
        WebGLContext  -> src0
  liftIO $ putStrLn $ "\n" ++ src1
  EitherT $ compileShader gl shadertype src1


compileIxProgram
  :: forall (ctx :: GLContext) gl m vs j x y.
     ( IsGLES m gl 
     , Binding vs [Maybe String]
     , Enum (GLUint gl)
     , HasContext ctx
     , MonadIO m 
     )
  => gl 
  -> IxShader ctx '[] (vs :: [*]) x
  -> IxShader ctx '[] j y
  -> m (Either String (GLProgram gl))
compileIxProgram gl ixvertex ixfragment = runEitherT $ do
  let GLES{..} = gles gl
  vshader <- EitherT $ compileIxShader gl ixvertex gl_VERTEX_SHADER
  fshader <- EitherT $ compileIxShader gl ixfragment gl_FRAGMENT_SHADER
  let attribNames = catMaybes $ getVertexBinding @vs
      attribLocs  = [0 ..]
      attribs     = zip attribLocs attribNames
  program <- EitherT $ compileProgram gl attribs [vshader, fshader]
  lift $ do
    glUseProgram program
    mapM_ glDeleteShader [vshader, fshader]
  return program

