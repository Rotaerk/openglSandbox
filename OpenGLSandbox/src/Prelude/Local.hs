module Prelude.Local where

import Control.Monad.IO.Class

ioPutStrLn :: MonadIO io => String -> io ()
ioPutStrLn s = liftIO $ putStrLn s
