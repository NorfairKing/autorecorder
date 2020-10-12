{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AutoRecorder.OptParse.Types where

import AutoRecorder.Input
import AutoRecorder.Output
import Data.Word
import Path

data Arguments = Arguments Command Flags
  deriving (Show, Eq)

data Command = CommandRecord RecordFlags
  deriving (Show, Eq)

data RecordFlags
  = RecordFlags
      { recordFlagSpecFile :: FilePath,
        recordFlagOutputFile :: FilePath,
        recordFlagSpeed :: Maybe Double,
        recordFlagRows :: Maybe Word16,
        recordFlagColumns :: Maybe Word16,
        recordFlagDefaultRows :: Maybe Word16,
        recordFlagDefaultColumns :: Maybe Word16,
        recordFlagWorkingDir :: Maybe FilePath,
        recordFlagMistakeProbability :: Maybe Mistakes,
        recordFlagOutputView :: Maybe OutputView,
        recordFlagCleanup :: Maybe Bool
      }
  deriving (Show, Eq)

data Flags
  = Flags
  deriving (Show, Eq)

data Configuration

data Environment
  = Environment
      { envAsciinemaConfigDir :: Maybe FilePath
      }
  deriving (Show, Eq)

data Instructions = Instructions Dispatch Settings
  deriving (Show, Eq)

data Dispatch = DispatchRecord RecordSettings
  deriving (Show, Eq)

data RecordSettings
  = RecordSettings
      { recordSetSpecFile :: Path Abs File,
        recordSetOutputFile :: Path Abs File,
        recordSetSpeed :: Double,
        recordSetRows :: Maybe Word16,
        recordSetColumns :: Maybe Word16,
        recordSetDefaultRows :: Word16,
        recordSetDefaultColumns :: Word16,
        recordSetWorkingDir :: Maybe (Path Abs Dir),
        recordSetMistakes :: Mistakes,
        recordSetOutputView :: OutputView,
        recordSetCleanup :: Bool
      }
  deriving (Show, Eq)

data Settings
  = Settings
  deriving (Show, Eq)
