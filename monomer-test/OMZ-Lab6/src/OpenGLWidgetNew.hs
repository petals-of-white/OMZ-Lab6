{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module OpenGLWidgetNew (
  openGLWidget
) where

import           Control.Lens
import           Control.Monad
import           Data.Default
import           Data.Typeable                (cast)
import           Data.Vector.Storable         (Vector)
import           Foreign.Ptr
import           Foreign.Storable

import qualified Data.Vector.Storable         as V

import           Data.Convertible
import           Foreign.Marshal              (withArrayLen)
import           Graphics.Rendering.OpenGL    (errors)
import           Graphics.Rendering.OpenGL.GL as GL hiding (color)
import           Linear
import           Linear.OpenGL                (m44GLmatrix)
import           Monomer
import qualified Monomer.Lens                 as L
import           Monomer.Widgets.Single

data OpenGLWidgetMsg
  = OpenGLWidgetInit Program VertexArrayObject BufferObject TextureObject | OpenGLWidgetError String
  deriving (Show, Eq)

data OpenGLWidgetState = OpenGLWidgetState {
  _ogsProgram   :: Program,
  _ogsVao       :: VertexArrayObject,
  _ogsVbo       :: BufferObject,
  _ogsTexture2D :: TextureObject
} deriving (Show, Eq)


normalizePeak :: (Num a, Real a, Convertible Double a) => a -> a -> a -> a -> a -> a
normalizePeak minP maxP newMin newMax value =
  convert $ (realToFrac newMin :: Double) + realToFrac (value - minP) / realToFrac (maxP - minP) * realToFrac (newMax - newMin)

writeLog :: String -> IO ()
writeLog = appendFile "log.txt" . flip (++) "\n"

openGLWidget :: (Storable p, Show p, Ord p, Bounded p, Real p, Convertible Double p) => [p] -> Int -> Int -> M44 Float -> WidgetNode s e
openGLWidget img rows cols transMatrix = defaultWidgetNode "openGLWidget" widget where
  widget = makeOpenGLWidget img rows cols transMatrix glState
  glState = Nothing

makeOpenGLWidget :: (Storable p, Show p, Ord p, Bounded p, Real p, Convertible Double p) => [p] -> Int -> Int -> M44 Float -> Maybe OpenGLWidgetState -> Widget s e
makeOpenGLWidget img rows cols transMat state = widget where
  widget = createSingle state def {
    singleInit = initialize,
    singleMerge = merge,
    singleDispose = dispose,
    singleHandleMessage = handleMessage,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  initialize _wenv node = resultReqs node reqs where
    widgetId = node ^. L.info . L.widgetId
    path = node ^. L.info . L.path

    floatSize = sizeOf (undefined :: Float)
    initOpenGL = do
      -- This needs to run in render thread
      debugOutput $= Enabled
      debugMessageCallback $= Just (\dmsg@(DebugMessage _ dtype _ _ msg) ->
          case dtype of
            DebugTypeError              -> writeLog (show dmsg)
            DebugTypeUndefinedBehavior  -> writeLog (show dmsg)
            DebugTypeDeprecatedBehavior -> writeLog (show dmsg)
            _                           -> writeLog (show msg)

        )

      glVersion >>= writeLog

      vbo <- genObjectName
      vao <- genObjectName

      bindVertexArrayObject $= Just vao

      bindBuffer ArrayBuffer $= Just vbo


      bufferData ArrayBuffer $= (fromIntegral (floatSize * 4 * 6), nullPtr, StaticDraw)

      -- Creating texture
      tex2D <- genObjectName
      textureBinding Texture2D $= Just tex2D


      textureFilter Texture2D $= ((Linear', Nothing), Linear')
      textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
      textureWrapMode Texture2D T $= (Repeated, ClampToEdge)


      let normalizedHistogram =  map (normalizePeak (minimum img) (maximum img) minBound maxBound) img

      withArrayLen normalizedHistogram (\ len ptr ->
        let pixData = PixelData Red UnsignedShort ptr in
        writeLog ("Len is: " ++ show len) >>
        texImage2D Texture2D NoProxy 0 R16 (TextureSize2D (fromIntegral rows) (fromIntegral cols)) 0 pixData
        )

      errors >>= writeLog . show
      textureErorrs <- debugLoggedMessages
      writeLog $ "Errors after setting vertex textures: " ++ show textureErorrs


      program <- createShaderProgram

      currentProgram $= Just program

      -- Uniforms
      samplerUniform <- get $ uniformLocation program "tex2d_sampler"
      transMatUniform <- get $ uniformLocation program "u_transform"

      writeLog $ "founb uniforms :" ++ "sampler2D: " ++ show samplerUniform ++ " TransMat: " ++ show transMatUniform
      uniform samplerUniform $= (0 :: GLint)
      writeLog "sampler set"
      uniform transMatUniform $= transMat ^. m44GLmatrix
      writeLog "transmat set"
      uniformErrors <- debugLoggedMessages
      writeLog $ "Uniform errors: " ++ show uniformErrors

      currentProgram $= Nothing
      textureBinding Texture2D $= Nothing


      return $ OpenGLWidgetInit program vao vbo tex2D

    reqs = [RunInRenderThread widgetId path initOpenGL]


  merge _wenv node _oldNode oldState = resultNode newNode where
    newNode = node
           & L.widget .~ makeOpenGLWidget img rows cols transMat oldState

  dispose _wenv node = resultReqs node reqs where

    widgetId = node ^. L.info . L.widgetId
    path = node ^. L.info . L.path
    disposeOpenGL = do
      case state of
        Just (OpenGLWidgetState programId vao vbo tex2d)  -> do
          writeLog "Disposing..."
          deleteObjectName tex2d
          deleteObjectName vao
          deleteObjectName vbo
          deleteObjectName programId
          writeLog "disposed"
        Nothing -> pure ()

    reqs = [RunInRenderThread widgetId path disposeOpenGL]

  handleMessage _wenv node _target msg = case cast msg of
    Just (OpenGLWidgetInit shaderId vao vbo tex2D) -> Just result where
      newState = Just (OpenGLWidgetState shaderId vao vbo tex2D)
      newNode = node
        & L.widget .~ makeOpenGLWidget img rows cols transMat newState
      result = resultReqs newNode [RenderOnce]
    _ -> Nothing

  getSizeReq _wenv _node = (sizeReqW_, sizeReqH_) where
    sizeReqW_ = width (fromIntegral cols)
    sizeReqH_ = height (fromIntegral rows)

  render wenv node renderer_ =
    case state of
      Just actualState -> do
        createRawTask renderer_ (do

          doInScissor winSize dpr offset activeVp $
            let vertices =
                  V.fromList $ concat
                  [ [newX,newY, u, v] | (x,y,u,v) <-textureCoords,
                    let (newX,newY) = viewportToWindow winSize nodeVp (x,y)] in do

            drawVertices transMat actualState (V.map realToFrac vertices :: Vector Float)
          )
      Nothing -> writeLog "Nothing to Render."
    where
      dpr = wenv ^. L.dpr
      winSize = wenv ^. L.windowSize
      activeVp = wenv ^. L.viewport
      style = currentStyle wenv node
      nodeVp = getContentArea node style
      textureCoords = [ (-1, -1 ,0, 0), (-1, 1, 0, 1), (1,1, 1, 1),
                      (1, 1, 1, 1), (1,-1,1,0), (-1,-1,0,0)]
      offset = wenv ^. L.offset

doInScissor :: Monomer.Size -> Double -> Point -> Monomer.Rect -> IO () -> IO ()
doInScissor winSize dpr offset vp action = do

  -- OpenGL's Y axis increases from bottom to top
  scissor $= Just (Position (round $ rx+ox) (round $ winH - ry - oy - rh), GL.Size (round rw) (round rh))
  action
  scissor $= Nothing
  where
    winH = winSize ^. L.h * dpr
    Monomer.Point ox oy = mulPoint dpr offset
    Rect rx ry rw rh = mulRect dpr vp

-- | Перетворення координат Monomer до координат OpenGL
monomerToGl :: Monomer.Size -> Monomer.Point -> (Double, Double)
monomerToGl (Monomer.Size winW winH) (Monomer.Point x y) = (glX, glY)
  where
    glX = -1 + 2 * x / winW
    glY = -1 + 2 * (winH - y) / winH

viewportToWindow :: Monomer.Size -> Monomer.Rect -> (Double, Double) -> (Double, Double)
viewportToWindow winSize@(Monomer.Size winW winH) _nodeContentArea@(Rect rx ry rw rh) (x,y) =
  let (transX, transY) = monomerToGl winSize (Monomer.Point (rx + rw/2) (ry + rh/2)) in
  ((x - transX) * rw/winW, (y - transY) * rh/winH)


drawVertices :: M44 Float -> OpenGLWidgetState -> Vector Float -> IO ()
drawVertices transmat state vertices = do
  bindVertexArrayObject $= Just vao

  bindBuffer ArrayBuffer $= Just vbo

  V.unsafeWith vertices $ \vertsPtr ->

        bufferSubData ArrayBuffer WriteToBuffer 0 (fromIntegral (V.length vertices * floatSize)) vertsPtr



  vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 2 Float (fromIntegral (floatSize * 4)) nullPtr)
  vertexAttribArray (AttribLocation 0) $= Enabled


      -- UV texture coords
  vertexAttribPointer (AttribLocation 1) $=
    (ToFloat, VertexArrayDescriptor 2 Float (fromIntegral (floatSize * 4)) (nullPtr `plusPtr` (floatSize * 2)))

  vertexAttribArray (AttribLocation 1) $= Enabled
  textureBinding Texture2D $= Just tex

  currentProgram $= Just shaderId

  bindVertexArrayObject $= Just vao
  transMatUniform <- get $ uniformLocation shaderId "u_transform"
  uniform transMatUniform $= transmat ^. m44GLmatrix


  drawArrays Triangles 0 6

  where
    floatSize = sizeOf (undefined :: Float)
    OpenGLWidgetState shaderId vao vbo tex = state



createShaderProgram :: IO Program
createShaderProgram = do
  shaderProgram <- GL.createProgram
  vertShader <- makeShader VertexShader "shaders/vert.glsl"
  fragShader <- makeShader FragmentShader "shaders/frag.glsl"

  attachShader shaderProgram vertShader
  attachShader shaderProgram fragShader

  linkProgram shaderProgram
  checkProgramLink shaderProgram

  validateProgram shaderProgram
  checkProgramValidation shaderProgram

  deleteObjectNames [vertShader, fragShader]

  return shaderProgram

checkProgramValidation :: Program -> IO ()
checkProgramValidation shaderProgram = do
  validated <- get (validateStatus shaderProgram)

  if not validated then do
    infolog <- programInfoLog shaderProgram
    writeLog infolog
  else
    writeLog "Program validated"

checkProgramLink :: Program -> IO ()
checkProgramLink shaderProgram = do
  linked <- get (linkStatus shaderProgram)
  if not linked then do
    infolog <- programInfoLog shaderProgram
    writeLog infolog
  else
    writeLog "Program linked"

  unless linked $ do
    infolog <- programInfoLog shaderProgram
    writeLog infolog


makeShader :: ShaderType -> FilePath -> IO Shader

makeShader shType shaderFile = do
  shader <- createShader shType
  src <- readFile shaderFile
  shaderSourceBS shader $= packUtf8 src

  compileShader shader
  checkShaderCompile shader

  return shader

checkShaderCompile :: Shader -> IO ()
checkShaderCompile shader = do
  compiled <- get shaderCompiler
  if not compiled then do
    infolog <- shaderInfoLog shader
    writeLog infolog
  else
    writeLog "shader compiled"

