{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AutoRecorder.OptParse
  ( module AutoRecorder.OptParse,
    module AutoRecorder.OptParse.Types,
  )
where

import AutoRecorder.Input
import AutoRecorder.OptParse.Types
import AutoRecorder.Output
import Data.Maybe
import Data.Version
import Options.Applicative
import Path.IO
import Paths_autorecorder
import qualified System.Environment as System

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  combineToSettings flags

combineToSettings :: Flags -> IO Settings
combineToSettings Flags {..} = do
  let settingSpeed = fromMaybe 1 flagSpeed
  settingSpecFile <- resolveFile' flagSpecFile
  settingOutputFile <- resolveFile' flagOutputFile
  let settingRows = flagRows
  let settingColumns = flagColumns
  let settingDefaultRows = fromMaybe 25 flagDefaultRows
  let settingDefaultColumns = fromMaybe 80 flagDefaultColumns
  settingWorkingDir <- mapM resolveDir' flagWorkingDir
  let settingMistakes = fromMaybe (MistakesWithProbability 0.02) flagMistakeProbability
  let settingOutputView = fromMaybe DisplayOutputView flagOutputView
  let settingCleanup = fromMaybe True flagCleanup
  pure Settings {..}

getFlags :: IO Flags
getFlags = do
  args <- System.getArgs
  let result = runFlagsParser args
  handleParseResult result

runFlagsParser :: [String] -> ParserResult Flags
runFlagsParser = execParserPure prefs_ argumentsParser
  where
    prefs_ =
      defaultPrefs
        { prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True
        }

argumentsParser :: ParserInfo Flags
argumentsParser = info (helper <*> parseFlags) help_
  where
    help_ = fullDesc <> progDesc description
    description = "ASCIInema autorecorder version " <> showVersion version

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> strArgument
      ( mconcat
          [ help "The instructions file",
            metavar "SPEC_FILE"
          ]
      )
    <*> strArgument
      ( mconcat
          [ help "The output file",
            metavar "OUTPUT_FILE"
          ]
      )
    <*> parseSpeedFlag
    <*> optional (option auto (mconcat [help "The number of rows. This overrides what is in the spec file or in the current terminal.", metavar "ROWS", long "rows"]))
    <*> optional (option auto (mconcat [help "The number of columns. This overrides what is in the spec file or in the current terminal.", metavar "COLUMNS", long "columns"]))
    <*> optional (option auto (mconcat [help "The default number of rows. This overrides what is in the current terminal but not what is in the spec file.", metavar "ROWS", long "default-rows"]))
    <*> optional (option auto (mconcat [help "The default number of columns. This overrides what is in the current terminal but not what is in the spec file.", metavar "COLUMNS", long "default-columns"]))
    <*> optional (strOption (mconcat [help "The working directory to record the cast in", metavar "DIRECTORY", long "working-dir"]))
    <*> parseMistakesFlag
    <*> parseOutputViewFlag
    <*> optional (flag True False (mconcat [help "Don't clean up after the cast", long "no-cleanup"]))

parseSpeedFlag :: Parser (Maybe Double)
parseSpeedFlag = optional $ option auto $ mconcat [long "speed", help "The speed multiplier", metavar "DOUBLE"]

parseMistakesFlag :: Parser (Maybe Mistakes)
parseMistakesFlag =
  optional $
    (MistakesWithProbability <$> option auto (mconcat [long "mistakes", help "Enable mistakes"]))
      <|> flag' NoMistakes (mconcat [long "no-mistakes", help "Disable mistakes"])

parseOutputViewFlag :: Parser (Maybe OutputView)
parseOutputViewFlag =
  optional $
    flag' DisplayOutputView (mconcat [long "display", help "Display what is happening"])
      <|> flag' DebugOutputView (mconcat [long "debug", help "Debug the input and output messages"])
      <|> flag' NoOutputView (mconcat [long "no-output", help "Do not show what is happening"])
      <|> flag' ProgressOutputView (mconcat [long "progress", help "Only show the progress of what is happening"])
