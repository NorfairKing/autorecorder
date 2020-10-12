{-# LANGUAGE RecordWildCards #-}

{-# CFILES window_size.c #-}

module AutoRecorder.WindowSize where

import Data.Word
import Foreign.C.Types (CInt (..), CLong (..), CShort (..))
import System.Posix.Types (Fd (..))

data WindowSize
  = WindowSize
      { windowSizeRows :: Word16,
        windowSizeColumns :: Word16
      }
  deriving (Show, Eq)

foreign import ccall "window_size.h c_get_window_size" c_getWindowSize :: Fd -> IO CLong

getWindowSize :: Fd -> IO WindowSize
getWindowSize fd = do
  (a, b) <- (`divMod` 65536) `fmap` c_getWindowSize fd
  let windowSizeRows = fromIntegral a
  let windowSizeColumns = fromIntegral b
  return WindowSize {..}

foreign import ccall "window_size.h c_set_window_size" c_setWindowSize :: Fd -> CShort -> CShort -> IO ()

setWindowSize :: Fd -> WindowSize -> IO ()
setWindowSize fd WindowSize {..} = c_setWindowSize fd (fromIntegral windowSizeRows) (fromIntegral windowSizeColumns)
