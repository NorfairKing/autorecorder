module AutoRecorder
  ( autoRecorder,
  )
where

import AutoRecorder.Commands
import AutoRecorder.OptParse

autoRecorder :: IO ()
autoRecorder = do
  Instructions d Settings <- getInstructions
  case d of
    DispatchRecord rs -> record rs
