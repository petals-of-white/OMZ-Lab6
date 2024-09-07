module Graphics.Render where
import           AppState
import           Control.Monad.Except
import           Control.Monad.Exception
import qualified Data.ByteString              as BS
import qualified Data.Text.Lazy               as LazyText
import           Data.Word
import           Graphics.GPipe
-- import           Graphics.GPipe.Context.GLFW
import           Data.Binary
import qualified Data.ByteString.Lazy         as LBS
import           Data.Maybe                   (fromMaybe)
import           Graphics.GL
import           Graphics.GPipe.Context.GLFW  as GLFW
import           Graphics.Rendering.OpenGL.GL (Capability (..),
                                               debugMessageCallback,
                                               debugOutput, ($=))
import           Graphics.Shaders             as Shaders
import           Graphics.Texture
import           Graphics.UI.GLFW             (WindowHint (..))
import           Graphics.Uniforms
import           Input
import           Typograffiti

data GraphicsIO ctx os m i = GraphicsIO {
  copyOriginal      :: ContextT ctx os m (),
  drawWindowLevel   :: Render os (),
  drawBinarized     :: Render os (),
  renderTargetToWin :: Render os (),
  renderInfo        :: State i -> ExceptT TypograffitiError (ContextT ctx os m) (),
  drawNormalized :: Render os ()
  }

app :: Format RWord -> Size2 -> BS.ByteString -> TransOptions Float -> IO ()
app format (V2 row col) imgBytes (Windowing {ww=w, wl=l}, BinTreshold b, _norm@(Bounds normMin normMax), scaling) = do
    let winFormat = WindowFormatColor RGB32F
        imgLength = row * col
        imgWord = (map byteSwap16 . decode . LBS.append (encode imgLength) . LBS.fromStrict) imgBytes :: [Word16]
        minV = Prelude.minimum imgWord
        maxV = Prelude.maximum imgWord
        floatToImgRange = round . realToFrac . (*) (realToFrac maxV)
        floatToWord16Range = round . realToFrac . (*) (realToFrac (maxBound :: Word16))
        wordTransOptions =
          ( Windowing {ww=floatToImgRange w, wl=floatToImgRange l},
            BinTreshold (floatToImgRange b),
            Bounds (floatToWord16Range normMin) (floatToWord16Range normMax),
            scaling)


    void $
      runContextT GLFW.defaultHandleConfig $ runExceptT $ do
            lift $ do

                win <- newWindow winFormat $ (GLFW.defaultWindowConfig "Lab2") {configWidth=row*2, configHeight=col*2, configHints = [WindowHint'OpenGLDebugContext True]}
                debugOutput $= Enabled
                debugMessageCallback $= Just print
                vertexBuffer1 :: Buffer os (B2 Float, B2 Float) <- newBuffer 4
                vertexBuffer2 :: Buffer os (B2 Float, B2 Float) <- newBuffer 4
                uniformFloatBuffer :: Buffer os (Uniform (B Word32)) <- newBuffer 1
                uniform2FloatBuffer :: Buffer os (Uniform (B Word32, B Word32)) <- newBuffer 1
                uniform4FloatBuffer :: Buffer os (Uniform (B Word32, B Word32, B Word32, B Word32)) <- newBuffer 1

                let binarizeUni = (uniformFloatBuffer, 0)
                    windowLevelUni = (uniform2FloatBuffer, 0)
                    normalizeUni = (uniform4FloatBuffer, 0)

                writeBuffer vertexBuffer1 0 vertices1
                writeBuffer vertexBuffer2 0 vertices2

                -- Textures
                let texSize = V2 row col
                originalTex <- newTexture2D format texSize 1
                outputTex <- newTexture2D format texSize 1
                floatTex <- newTexture2D R32F texSize 1

                writeTexture2D originalTex 0 0 texSize imgWord
                writeTexture2D outputTex 0 0 texSize imgWord
                writeTexture2D floatTex 0 0 texSize (repeat 0.3 :: [Float])

                drawFunc <- liftIO drawTextFunc
                case drawFunc of
                  (Left err) -> error $ "Помилка під час спроби створити функцію виведення тексту. " ++ show err
                  (Right func) -> do
                    let drawImgInfo state  = do
                          hoveredPixCoords <- lift $ Input.coordsUnderCursor win ViewPort {viewPortSize=V2 row col, viewPortLowerLeft=0}
                          (minMax, pixUnderCursor) <- ExceptT (Right <$> Graphics.Texture.retrieveInfo outputTex hoveredPixCoords)
                          glDisable GL_SCISSOR_TEST
                          glViewport 0 0 (fromIntegral row) (fromIntegral (col*2))
                          imageInfo func [move 10 10, TextTransformMultiply (V4 1 1 1 1)]
                            (V2 row (col*2)) ImageInfo {currentPeaks=minMax, transState=state, hoveredPixel=pixUnderCursor}

                    let blend = NoBlending
                    shaderBinarize <- compileShader Shaders.binarize
                    liftIO $ putStrLn "Binary shader compiled"
                    shaderWindowLevel <- compileShader Shaders.windowLevel
                    liftIO $ putStrLn "Window Level shader compiled"
                    shaderRenderToWindow <- compileShader $ Shaders.texToWindow (realToFrac scaling)
                    liftIO $ putStrLn "Texture-Window shader compiled"
                    shaderNormalize <- compileShader Shaders.normalize
                    liftIO $ putStrLn "Normalization shader compiled"

                    let rendBin = do
                          vertexArray <- newVertexArray vertexBuffer1
                          target <- getTexture2DImage outputTex 0
                          let shaderEnv = ShaderEnvironment {
                                primitives = toPrimitiveArray TriangleStrip vertexArray,
                                colorImage=target,
                                clrMask = True,
                                tex2D = originalTex,
                                window = win,
                                blending=blend
                                }
                          shaderBinarize (shaderEnv, binarizeUni)

                        rendWinLevel = do
                          vertexArray <- newVertexArray vertexBuffer1
                          target <- getTexture2DImage outputTex 0
                          let shaderEnv = ShaderEnvironment {
                                primitives = toPrimitiveArray TriangleStrip vertexArray,
                                colorImage=target,
                                clrMask = True,
                                tex2D = originalTex,
                                window = win,
                                blending=blend
                                }

                          shaderWindowLevel (shaderEnv, windowLevelUni)

                        rendNormalized = do
                          vertexArray <- newVertexArray vertexBuffer1
                          target <- getTexture2DImage outputTex 0
                          let shaderEnv = ShaderEnvironment {
                                primitives = toPrimitiveArray TriangleStrip vertexArray,
                                colorImage=target,
                                clrMask = True,
                                tex2D = originalTex,
                                window = win,
                                blending=blend
                                }

                          shaderNormalize (shaderEnv, normalizeUni)


                        graphicsIO = GraphicsIO {
                          copyOriginal = writeTexture2D outputTex 0 0 texSize imgWord,
                          drawNormalized = rendNormalized,
                          drawBinarized = rendBin,
                          drawWindowLevel = rendWinLevel,
                          renderTargetToWin = do
                            vertexArray <- newVertexArray vertexBuffer1
                            let shaderEnv = ShaderEnvironment {
                                primitives = toPrimitiveArray TriangleStrip vertexArray,
                                colorImage= undefined,
                                clrMask = True,
                                tex2D = outputTex,
                                window = win,
                                blending=blend
                                }
                            shaderRenderToWindow shaderEnv
                            ,
                          renderInfo = drawImgInfo
                        }
                    loop win
                      graphicsIO
                      Uniforms {binaryUni = binarizeUni, windowLevelUni = windowLevelUni, normUni=normalizeUni}
                      State {imgMode=Original, transOptions = wordTransOptions, originalPeaks = Bounds {minValue=minV, maxValue=maxV}}
                      True


loop :: (MonadIO m,  MonadException m, MonadAsyncException m, Bounded i, Integral i, Show i) =>
  Window os RGBFloat ds -> GraphicsIO GLFW.Handle os m i  ->  Uniforms os -> State i -> Bool -> ContextT GLFW.Handle os m ()

loop win graphicsIO uniforms
  state@State {imgMode, originalPeaks=peaks@Bounds{minValue=minPeak, maxValue=maxPeak},
  transOptions=trans@(Windowing {ww, wl}, BinTreshold t, _norm@Bounds{minValue=normMin, maxValue=normMax}, _)}  shouldUpdate =

  do
  render $ clearWindowColor win 0
  when shouldUpdate $ do
      let writeUniform loc value = uncurry writeBuffer loc [value]

      (case imgMode of
          Original -> do
            copyOriginal graphicsIO
          Binarized  -> do
            writeUniform (binaryUni uniforms) (fromIntegral t)
            render $ drawBinarized graphicsIO
          WindowLevel -> do
            writeUniform (windowLevelUni uniforms) (fromIntegral wl, fromIntegral ww)
            render $ drawWindowLevel graphicsIO

          AppState.Normalized -> do
            writeUniform (normUni uniforms) (fromIntegral minPeak, fromIntegral maxPeak, fromIntegral normMin, fromIntegral normMax)
            render $ drawNormalized graphicsIO
              )

  render $ renderTargetToWin graphicsIO

  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
  textDrawRes <- runExceptT (renderInfo graphicsIO state)
  case textDrawRes of
    Left e   -> liftIO $ putStrLn $ "Error while drawing text!!!" ++ show e
    Right () -> return ()

  newMode <- fromMaybe imgMode <$> Input.keyModeChanged win

  swapWindowBuffers win
  closeRequested <- GLFW.windowShouldClose win

  unless (closeRequested == Just True) $
    loop win graphicsIO uniforms State {imgMode=newMode, transOptions=trans, originalPeaks=peaks} (imgMode /= newMode)

vertices1, vertices2 :: [(V2 Float, V2 Float)]

vertices1 =  [(V2 (-1) (-1), V2 0 0),  (V2 1 (-1), V2 1 0),
              (V2 (-1) 1, V2 0 1),     (V2 1 1, V2 1 1)]

vertices2 = [(V2 (-1) (-1), V2 0 0),  (V2 1 (-1), V2 1 0),
             (V2 (-1) 1, V2 0 1),     (V2 1 1, V2 1 1)]

drawTextFunc :: (MonadIO m, MonadFail m, MonadError TypograffitiError m) =>
  IO (Either TypograffitiError (RichText -> m (AllocatedRendering [TextTransform])))
drawTextFunc =  makeDrawText' fontPath 0 (PixelSize 15 15) (defaultSample { sampleText = textSample, minLineHeight=20.0 })


-- | Renders all image info to the screen
imageInfo :: (MonadIO m, MonadFail m, Show a, Num a, MonadError TypograffitiError m) =>
    (RichText -> m (AllocatedRendering [TextTransform])) -> [TextTransform] -> V2 Int -> ImageInfo a -> m ()

imageInfo drawFunc transforms size
  ImageInfo {
    currentPeaks = Bounds {minValue=minV, maxValue=maxV},
    transState= State{
      imgMode,
      transOptions=(Windowing {ww, wl}, BinTreshold b, _norm@Bounds{minValue=normMin, maxValue=normMax}, _),
      originalPeaks=Bounds {minValue=_, maxValue=_}
    },
    hoveredPixel
  } = do
    let infoText = [
            "Поточний мін: " ++ show minV,
            "Поточний макс: " ++ show maxV,
            "Режим: " ++ show imgMode,
            "Вікно-рівень: l=" ++ show wl ++ " w=" ++ show ww,
            "Поріг бінаризації: " ++ show b,
            "Нормалізація: від " ++ show normMin ++ " до " ++ show normMax
            -- "Нормалізація: minP=" ++ show minP ++ " maxP=" ++ show maxP ++ ""
            ] ++ (case hoveredPixel of Just p -> ["Піксель під курсором: " ++ show p]; Nothing -> [])
    drawText' <- drawFunc $ str $ Prelude.unlines infoText
    liftIO $ arDraw drawText' transforms size


fontPath :: FilePath
fontPath = "assets\\Lora-Regular.ttf"

textSample :: LazyText.Text
textSample = LazyText.pack $ Prelude.unlines [
            "0.,123456789",
            "Поточний мін: ",
            "Поточний макс: ",
            "Режим: " ++ show Original ++ show Binarized ++ show WindowLevel ++ show AppState.Normalized,
            "Вікно-рівень: l= w=" ,
            "Поріг бінаризації: ",
            "Масштабування: ",
            "Нормалізація: від 0.,123456789 до 0.,123456789",
            "Піксель під курсором: 0.,123456789"

    ]
