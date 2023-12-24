{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude.Local

import Paths_OpenGLSandbox

import ApplicationException
import Control.Exception
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Acquire.Local
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Function
import Data.Functor
import Data.IORef
import Data.Primitive.PrimArray
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils as FMU
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL.Core33
import qualified Graphics.UI.GLFW.Local as GLFW
import System.Clock

main :: IO ()
main = do
  runResourceT resourceMain
  `catch` (
    \(e :: ApplicationException) ->
      putStrLn $ displayException e
  )
  `catch` (
    \(e :: GLFW.GLFWException) ->
      putStrLn $ displayException e
  )

resourceMain :: ResourceT IO ()
resourceMain = do
  liftIO $ GLFW.setErrorCallback . Just $ \errorCode errorMessage ->
      putStrLn $ "GLFW error callback: " ++ show errorCode ++ " - " ++ errorMessage
  ioPutStrLn "GLFW error callback set."

  allocateAcquire_ GLFW.acquireInitialization
  ioPutStrLn "GLFW initialized."

  liftIO $ do
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
    GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
  window <- allocateAcquire_ $ GLFW.acquireWindow 800 600 "OpenGL Sandbox"
  ioPutStrLn "Window created."

  liftIO $ GLFW.makeContextCurrent (Just window)
  ioPutStrLn "Attached window's GL context to the main thread."

  lastWindowResizeTimeRef <- liftIO $ do
    ref <- newIORef Nothing
    GLFW.setFramebufferSizeCallback window $ Just $ \_ _ _ -> do
      time <- getTime Monotonic
      writeIORef ref $ Just time
    pure ref
  ioPutStrLn "Window framebuffer size callback registered."

  shaderProgramId <- liftIO $ do
    vertShaderId <- compileShader "triangle.vert" GL_VERTEX_SHADER
    fragShaderId <- compileShader "triangle.frag" GL_FRAGMENT_SHADER
    shaderProgramId <- glCreateProgram
    glAttachShader shaderProgramId vertShaderId
    glAttachShader shaderProgramId fragShaderId
    glLinkProgram shaderProgramId
    linkingFailed <- alloca $ \linkStatusPtr -> do
      glGetProgramiv shaderProgramId GL_LINK_STATUS linkStatusPtr
      (GL_FALSE ==) . fromIntegral <$> peek linkStatusPtr
    when linkingFailed $ do
      let maxLen = 512
      infoLogText <- fmap T.decodeUtf8Lenient . BS.createUptoN maxLen $ \infoLogBsPtr ->
        alloca $ \actualLenPtr -> do
          glGetProgramInfoLog shaderProgramId (fromIntegral maxLen) actualLenPtr (castPtr infoLogBsPtr)
          fromIntegral <$> peek actualLenPtr
      throwAppExM $ "Shader program link error: " ++ T.unpack infoLogText
    glUseProgram shaderProgramId
    glDeleteShader vertShaderId
    glDeleteShader fragShaderId
    return shaderProgramId
  ioPutStrLn "Shader program created."

  vertexArrayId <- liftIO $ do
    (vertexArrayId, vertexArrayBufferId) <- alloca $ \idPtr ->
      (,) <$>
      (glGenVertexArrays 1 idPtr >> peek idPtr) <*>
      (glGenBuffers 1 idPtr >> peek idPtr)

    glBindVertexArray vertexArrayId
    glBindBuffer GL_ARRAY_BUFFER vertexArrayBufferId

    let numVertices = length triangleVertices
    verticesArray <- newAlignedPinnedPrimArray numVertices
    forM_ (zip [0..] triangleVertices) $ \(idx, vertex) ->
      writePrimArray verticesArray idx vertex
    void $ withMutablePrimArrayContents verticesArray $ \verticesArrayPtr -> do
      glBufferData GL_ARRAY_BUFFER (fromIntegral $ numVertices * sizeOf (undefined :: Float)) verticesArrayPtr GL_STATIC_DRAW
      return undefined -- works around bug in type signature

    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (fromIntegral $ 3 * sizeOf (undefined :: Float)) nullPtr
    glEnableVertexAttribArray 0

    glBindBuffer GL_ARRAY_BUFFER 0
    glBindVertexArray 0

    return vertexArrayId
  ioPutStrLn "Vertex array created."

  ioPutStrLn "Render loop starting."
  liftIO $ fix $ \renderLoop ->
    GLFW.getWindowStatus window lastWindowResizeTimeRef >>= \case
      GLFW.WindowResized -> do
        (width, height) <- GLFW.getFramebufferSize window
        glViewport 0 0 (fromIntegral width) (fromIntegral height)
        putStrLn $ "Resized viewport to " ++ show width ++ "x" ++ show height
        renderLoop
      GLFW.WindowClosed -> pure ()
      GLFW.WindowReady -> do
        whenM (GLFW.getKey window GLFW.Key'Escape <&> (== GLFW.KeyState'Pressed)) $
          GLFW.setWindowShouldClose window True
        glClearColor 0.2 0.3 0.3 1
        glClear GL_COLOR_BUFFER_BIT
        glUseProgram shaderProgramId
        glBindVertexArray vertexArrayId
        glDrawArrays GL_TRIANGLES 0 3
        GLFW.swapBuffers window
        renderLoop

triangleVertices :: [Float]
triangleVertices =
  [
    -0.5, -0.5, 0,
    0.5, -0.5, 0,
    0, 0.5, 0
  ]

compileShader :: FilePath -> GLenum -> IO GLuint
compileShader shaderFileName shaderType = do
  shaderSrc <- BS.readFile =<< getDataFileName ("shaders/" ++ shaderFileName)
  shaderId <- glCreateShader shaderType
  BS.unsafeUseAsCString shaderSrc $ \shaderSrcPtr ->
    FMU.with shaderSrcPtr $ \shaderSrcPtrPtr ->
      glShaderSource shaderId 1 shaderSrcPtrPtr nullPtr
  glCompileShader shaderId
  compilationFailed <- alloca $ \compileStatusPtr -> do
    glGetShaderiv shaderId GL_COMPILE_STATUS compileStatusPtr
    (GL_FALSE ==) . fromIntegral <$> peek compileStatusPtr
  when compilationFailed $ do
    let maxLen = 512
    infoLogText <- fmap T.decodeUtf8Lenient . BS.createUptoN maxLen $ \infoLogBsPtr ->
      alloca $ \actualLenPtr -> do
        glGetShaderInfoLog shaderId (fromIntegral maxLen) actualLenPtr (castPtr infoLogBsPtr)
        fromIntegral <$> peek actualLenPtr
    throwAppExM $ "Compile error for shader '" ++ shaderFileName ++ "': " ++ T.unpack infoLogText
  return shaderId
