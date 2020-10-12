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
import qualified Env
import Options.Applicative
import Path.IO
import Paths_autorecorder
import qualified System.Environment as System

getInstructions :: IO Instructions
getInstructions = do
  (Arguments cmd flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions cmd flags env config

combineToInstructions :: Command -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (CommandRecord RecordFlags {..}) Flags Environment {..} _ = do
  let recordSetSpeed = fromMaybe 1 recordFlagSpeed
  recordSetSpecFile <- resolveFile' recordFlagSpecFile
  recordSetOutputFile <- resolveFile' recordFlagOutputFile
  let recordSetRows = recordFlagRows
  let recordSetColumns = recordFlagColumns
  let recordSetDefaultRows = fromMaybe 25 recordFlagDefaultRows
  let recordSetDefaultColumns = fromMaybe 80 recordFlagDefaultColumns
  recordSetWorkingDir <- mapM resolveDir' recordFlagWorkingDir
  let recordSetMistakes = fromMaybe (MistakesWithProbability 0.03) recordFlagMistakeProbability
  let recordSetOutputView = fromMaybe DisplayOutputView recordFlagOutputView
  let recordSetCleanup = fromMaybe True recordFlagCleanup
  let d = DispatchRecord RecordSettings {..}
  pure (Instructions d Settings)

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration _ _ = pure Nothing

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser = Environment <$> optional (Env.var Env.str "ASCIINEMA_CONFIG_HOME" (Env.help "The directory for asciinema config files"))

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argumentsParser
  where
    prefs_ =
      defaultPrefs
        { prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True
        }

argumentsParser :: ParserInfo Arguments
argumentsParser = info (helper <*> parseArguments) help_
  where
    help_ = fullDesc <> progDesc description
    description = "ASCIInema auterecorder version " <> showVersion version

parseArguments :: Parser Arguments
parseArguments = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
  hsubparser $
    mconcat
      [ command "record" parseCommandRecord
      ]

parseCommandRecord :: ParserInfo Command
parseCommandRecord = info parser modifier
  where
    modifier = fullDesc <> progDesc "Record an asciinema"
    parser =
      CommandRecord
        <$> ( RecordFlags
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
            )

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

parseFlags :: Parser Flags
parseFlags = pure Flags
