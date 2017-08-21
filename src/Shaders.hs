{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Shaders where

import           Graphics.Gristle

passthruVertex
  :: forall (ctx :: GLContext). IsGLContext ctx
  => IxShader ctx '[] '[ In Xvec2 "position"
                       , In Xvec4 "color"
                       , Out Xvec4 "fcolor"
                       , Out Xvec4 "gl_Position"
                       , Main
                       ] ()
passthruVertex = do
  p     <- in_
  c     <- in_
  color <- out_
  glpos <- gl_Position
  main_ $ do
    color .= c
    glpos .= mkvec4 (x p) (y p) 0.0 1.0

passthruFragment
  :: forall (ctx :: GLContext). IsGLContext ctx
  => IxShader ctx '[] '[ In Xvec4 "fcolor"
                       , Out Xvec4 (GLFragName ctx)
                       , Main
                       ] ()
passthruFragment = do
  color <- in_ 
  frag  <- gl_FragColor
  main_ $ frag .= color 
