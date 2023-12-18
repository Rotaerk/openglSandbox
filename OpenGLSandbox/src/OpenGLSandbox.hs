{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude.Local

--import Paths_OpenGLSandbox

import Control.Exception
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Acquire.Local
import Data.Function
import Data.Functor
import Data.IORef
import qualified Data.Vector.Storable as V
import Foreign.Marshal.Alloc
import Foreign.Storable
import Graphics.GL.Core33
import qualified Graphics.UI.GLFW.Local as GLFW
import System.Clock

main :: IO ()
main = do
  runResourceT resourceMain
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

  vboId <- liftIO $ alloca $ \vboIdPtr -> do
    glGenBuffers 1 vboIdPtr
    peek vboIdPtr
  ioPutStrLn $ "Created VBO with ID " ++ show vboId

  liftIO $ glBindBuffer GL_ARRAY_BUFFER vboId
  ioPutStrLn "Bound new VBO to GL_ARRAY_BUFFER."

  liftIO $ V.unsafeWith triangleVertices $ \triangleVerticesPtr -> do
    glBufferData GL_ARRAY_BUFFER (fromIntegral $ V.length triangleVertices * sizeOf (undefined :: Float)) triangleVerticesPtr GL_STATIC_DRAW
  ioPutStrLn "Filled VBO."

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
        processFrame window
        GLFW.swapBuffers window
        renderLoop

processFrame :: GLFW.Window -> IO ()
processFrame window = do
  whenM (GLFW.getKey window GLFW.Key'Escape <&> (== GLFW.KeyState'Pressed)) $
    GLFW.setWindowShouldClose window True
  glClearColor 0.2 0.3 0.3 1
  glClear GL_COLOR_BUFFER_BIT

triangleVertices :: V.Vector Float
triangleVertices = V.fromList
  [
    -0.5, -0.5, 0,
    0.5, 0.5, 0,
    0, 0.5, 0
  ]
