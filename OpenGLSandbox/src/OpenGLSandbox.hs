{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude.Local

--import Paths_OpenGLSandbox

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Acquire.Local
import Data.Function
import Data.IORef
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

  ioPutStrLn "Render loop starting."
  liftIO $ fix $ \renderLoop ->
    GLFW.getWindowStatus window lastWindowResizeTimeRef >>= \case
      GLFW.WindowResized -> renderLoop
      GLFW.WindowClosed -> pure ()
      GLFW.WindowReady -> do
        GLFW.swapBuffers window
        renderLoop
