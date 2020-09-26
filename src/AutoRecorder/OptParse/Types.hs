{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AutoRecorder.OptParse.Types where

import AutoRecorder.Input
import AutoRecorder.Output
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
        recordFlagColumns :: Maybe Word,
        recordFlagRows :: Maybe Word,
        recordFlagMistakeProbability :: Maybe Mistakes,
        recordFlagOutputView :: Maybe OutputView
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
        recordSetColumns :: Word,
        recordSetRows :: Word,
        recordSetMistakes :: Mistakes,
        recordSetOutputView :: OutputView
      }
  deriving (Show, Eq)

data Settings
  = Settings
  deriving (Show, Eq)
