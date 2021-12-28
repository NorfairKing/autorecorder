module AutoRecorder
  ( autoRecorder,
  )
where

import AutoRecorder.Commands
import AutoRecorder.OptParse

autoRecorder :: IO ()
autoRecorder = do
  settings <- getSettings
  record settings
