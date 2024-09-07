{-# LANGUAGE TypeFamilies #-}

module Graphics.Shaders (ShaderEnvironment(..),Graphics.Shaders.normalize, binarize, windowLevel, texToWindow)

where

import           Data.Word         (Word16)
import           Graphics.GPipe
import           Graphics.Uniforms
import           Prelude           hiding ((<*))

-- | Common shader environment
data ShaderEnvironment os prim source target a = ShaderEnvironment
  {
    primitives :: PrimitiveArray prim a,
    colorImage :: Image (Format target),
    clrMask    :: Color target Bool,
    tex2D      :: Texture2D os (Format source),
    window     :: Window os RGBFloat (),
    blending   :: Blending
  }

-- | A shader that converts any texture to float fragments
texToFloat :: forall src os prim s c h.
  (ColorRenderable src, c ~ ColorElement src, s ~ S F c, Convert s, ColorElement src ~ h, Color src (S F h) ~ S F h,
  ConvertFloat (S F h) ~ S F Float, ConvertFloat s ~ S F Float) =>
  Shader os (ShaderEnvironment os prim src RFloat (B2 Float, B2 Float)) (FragmentStream (S F Float))

texToFloat = do
    (fragmentStream, sampler) <- mapShader (\ShaderEnvironment {primitives, tex2D} -> (primitives, tex2D)) common
    let sampleTexture = sample2D sampler SampleAuto Nothing Nothing
    let fragmentStream2 = fmap (toFloat . sampleTexture) fragmentStream
    return fragmentStream2


-- | Perform binarization of a Word-like texture given the threshold from the uniform.
-- Renders output to a texture
binarize ::  Shader os (ShaderEnvironment os prim RWord RWord (B2 Float, B2 Float), BinaryThresholdUni os) ()

binarize = do
  (fragmentStream, sampler) <- mapShader (\(ShaderEnvironment {primitives, tex2D}, _) -> (primitives, tex2D)) common
  uniform <- getUniform snd
  let sampleTexture = sample2D sampler SampleAuto Nothing Nothing
  let fragmentStream2 = fmap sampleTexture fragmentStream

  draw (blending . fst) fragmentStream2
    (\c ->
      let porih = uniform
          binarized = ifThenElse' (c >* porih) (fromIntegral (maxBound :: Word16)) 0 in
      drawColor (\(ShaderEnvironment{colorImage=img, clrMask=mask}, _) -> (img, mask, False) ) binarized)


-- | Perform normalization of a Word-like texture given the original peaks from the uniform.
-- Renders output to a texture
normalize :: Shader os (ShaderEnvironment os prim RWord RWord (B2 Float, B2 Float), NormalizeUni os) ()

normalize = do
  (fragmentStream, sampler) <- mapShader (\(ShaderEnvironment {primitives, tex2D}, _) -> (primitives, tex2D)) common
  (minP,maxP, newMin, newMax) <- getUniform snd
  let sampleTexture = sample2D sampler SampleAuto Nothing Nothing
  let fragmentStream2 = fmap sampleTexture fragmentStream

  draw (blending . fst) fragmentStream2
    (\i ->
      let transformed = newMin + toWord ((toFloat (i - minP) / toFloat (maxP - minP)) * toFloat (newMax - newMin)) in
      drawColor (\(ShaderEnvironment{colorImage=img, clrMask=mask}, _) -> (img, mask, False) ) transformed)



-- | Perform windowing (window-level transformation) of a Word-like texture given window level and window width from the uniform.
-- Renders output to a texture
windowLevel :: Shader os (ShaderEnvironment os prim RWord RWord (B2 Float, B2 Float), WindowLevelUni os) ()

windowLevel = do

  (fragmentStream, sampler) <- mapShader (\(ShaderEnvironment {primitives, tex2D}, _) -> (primitives, tex2D)) common
  (l,w) <- getUniform snd

  let sampleTexture = sample2D sampler SampleAuto Nothing Nothing
  let fragmentStream2 = fmap sampleTexture fragmentStream

  draw (blending . fst) fragmentStream2
    (\c ->
      let transformed =
            let minV = 0 :: S F Word
                maxV = 65535 :: S F Word
            in

            ifThenElse (c <=* l - (w `div'` 2))
              (const minV)
              (const $ ifThenElse (c <=* l + (w `div'` 2)) (const (minV + (c - l + (w `div'` 2)))) (const maxV) c)
              c
      in
      drawColor (\(ShaderEnvironment{colorImage=img, clrMask=mask}, _) -> (img, mask, False) ) transformed)


-- | Renders any texture to window, converting it to Float format beforehand and multiplying by scaling factor.
texToWindow ::
  (ColorRenderable src, c ~ ColorElement src, s ~ S F c, Convert s, ColorElement src ~ h, Color src (S F h) ~ S F h,
  ConvertFloat (S F h) ~ S F Float, ConvertFloat s ~ S F Float) =>
  S F Float -> Shader os (ShaderEnvironment os prim src RFloat (B2 Float, B2 Float)) ()

texToWindow scale = do
  floatStream <- texToFloat
  let floatStreamV =  fmap (pure . (* scale)) floatStream
  drawWindowColor
    (\ShaderEnvironment{clrMask=mask, window=win, blending=blend} -> (win, ContextColorOption blend (pure mask) ))
    floatStreamV



-- | common transformations for textures
common :: ColorRenderable c => Shader os (PrimitiveArray prim (B2 Float, B2 Float), Texture2D os (Format c)) (FragmentStream (V2 FFloat), Sampler2D (Format c))

common = do
  primitiveStream  <- toPrimitiveStream fst
  let primitiveStream2 = fmap (\(V2 x y, uv) -> (V4 x y 0 1, uv)) primitiveStream

  fragmentStream <- rasterize (\(_, tex) ->
                        let V2 rows columns = head $ texture2DSizes tex in
                        (FrontAndBack, ViewPort (V2 0 0) (V2 rows columns), DepthRange 0 1)) primitiveStream2


  let edge = (pure ClampToEdge, undefined)
  samp <- newSampler2D $ \(_, tex) -> (tex, SamplerNearest, edge)
  return (fragmentStream, samp)
