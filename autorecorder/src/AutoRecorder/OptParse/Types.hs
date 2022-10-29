{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AutoRecorder.OptParse.Types where

import AutoRecorder.Input
import AutoRecorder.Output
import Data.Word
import Path

data Flags = Flags
  { flagSpecFile :: !FilePath,
    flagOutputFile :: !FilePath,
    flagSpeed :: !(Maybe Double),
    flagRows :: !(Maybe Word16),
    flagColumns :: !(Maybe Word16),
    flagDefaultRows :: !(Maybe Word16),
    flagDefaultColumns :: !(Maybe Word16),
    flagWorkingDir :: !(Maybe FilePath),
    flagMistakeProbability :: !(Maybe Mistakes),
    flagOutputView :: !(Maybe OutputView),
    flagCleanup :: !(Maybe Bool)
  }
  deriving (Show, Eq)

data Settings = Settings
  { settingSpecFile :: !(Path Abs File),
    settingOutputFile :: !(Path Abs File),
    settingSpeed :: !Double,
    settingRows :: !(Maybe Word16),
    settingColumns :: !(Maybe Word16),
    settingDefaultRows :: !Word16,
    settingDefaultColumns :: !Word16,
    settingWorkingDir :: !(Maybe (Path Abs Dir)),
    settingMistakes :: !Mistakes,
    settingOutputView :: !OutputView,
    settingCleanup :: !Bool
  }
  deriving (Show, Eq)
