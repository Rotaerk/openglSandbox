{-# LANGUAGE LambdaCase #-}

module Graphics.UI.GLFW.Local (
  module Graphics.UI.GLFW,
  GLFWException(..),
  throwGLFWExceptionM,
  acquireInitialization,
  acquireWindow,
  WindowStatus(..),
  getWindowStatus
) where

import Control.Exception
import Control.Monad.Catch
import Control.Monad.Extra
import Data.Acquire.Local
import Data.IORef
import Graphics.UI.GLFW
import qualified Graphics.UI.GLFW as GLFW
import System.Clock as Clock

data GLFWException = GLFWException { glfwException'functionName :: String } deriving (Eq, Show, Read)

instance Exception GLFWException where
  displayException (GLFWException functionName) = "GLFWException: " ++ functionName ++ " failed."

throwGLFWExceptionM :: MonadThrow m => String -> m a
throwGLFWExceptionM = throwM . GLFWException

acquireInitialization :: Acquire ()
acquireInitialization =
  unlessM GLFW.init (throwGLFWExceptionM "init")
  `mkAcquire`
  const GLFW.terminate

acquireWindow :: Int -> Int -> String -> Acquire GLFW.Window
acquireWindow width height title =
  fromMaybeM (throwGLFWExceptionM "createWindow") (GLFW.createWindow width height title Nothing Nothing)
  `mkAcquire`
  GLFW.destroyWindow

data WindowStatus = WindowReady | WindowResized | WindowClosed

getWindowStatus :: GLFW.Window -> IORef (Maybe TimeSpec) -> IO WindowStatus
getWindowStatus window lastResizeTimeRef =
  GLFW.windowShouldClose window >>= \case
    True -> return WindowClosed
    False -> do
      GLFW.pollEvents
      currentTime <- Clock.getTime Monotonic
      -- GLFW sends many resize events during the resizing process, and doesn't say when the user is done resizing.
      -- Thus, only consider it resized after some time has passed since the last event.
      atomicModifyIORef lastResizeTimeRef $ \case
        Just lastResizeTime | currentTime - lastResizeTime >= resizeDelay -> (Nothing, WindowResized)
        v -> (v, WindowReady)
  where
    resizeDelay = fromNanoSecs (100 * 1000 * 1000) -- 100 milliseconds
