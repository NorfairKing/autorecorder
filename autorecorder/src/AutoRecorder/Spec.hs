{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AutoRecorder.Spec where

import Autodocodec
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Word

data ASCIInemaSpec
  = ASCIInemaSpec
      { asciinemaCommand :: Maybe String,
        asciinemaTimeout :: Int, -- Seconds
        asciinemaFiles :: [FilePath],
        asciinemaRows :: Maybe Word16,
        asciinemaColumns :: Maybe Word16,
        asciinemaWorkingDir :: Maybe FilePath,
        asciinemaEnvironment :: Map String String,
        asciinemaAllowFail :: Bool,
        asciinemaExpectExitCode :: Maybe Word8,
        asciinemaInput :: [ASCIInemaCommand]
      }
  deriving (Show, Eq)

instance HasCodec ASCIInemaSpec where
  codec =
    object "ASCIInemaSpec" $
      ASCIInemaSpec
        <$> optionalField "command" "The command to show off. Leave this to just run a shell" .= asciinemaCommand
        <*> optionalFieldWithDefault "timeout" 60 "How long to allow the recording to run before timing out, in seconds" .= asciinemaTimeout
        <*> optionalFieldWithOmittedDefaultWith "files" (singleOrListCodec codec) [] "The files that are being touched. These will be brought back in order afterwards." .= asciinemaFiles
        <*> optionalField "rows" "The number of rows (height) of the screen" .= asciinemaRows
        <*> optionalField "columns" "The number of columns (width) of the screen" .= asciinemaColumns
        <*> optionalField "working-dir" "The working directory directory" .= asciinemaWorkingDir
        <*> optionalFieldWithDefault "environment" M.empty "Variables to add to the environment" .= asciinemaEnvironment
        <*> optionalFieldWithDefault "allow-fail" False "Whether to allow a nonzero exit code" .= asciinemaAllowFail
        <*> optionalField "expect-exit-code" "The exit code to expect" .= asciinemaExpectExitCode
        <*> optionalFieldWithDefault "input" [] "The inputs to send to the command" .= asciinemaInput

data ASCIInemaCommand
  = Wait Word -- Milliseconds
  | SendInput String
  | Type String Word -- Milliseconds
  deriving (Show, Eq)

instance HasCodec ASCIInemaCommand where
  codec =
    dimapCodec f g
      $ eitherCodec
        (object "Wait" $ requiredField "wait" "How long to wait (in milliseconds)")
      $ eitherCodec
        (object "SendInput" $ requiredField "send" "The input to send")
        ( object "Type" $
            (,)
              <$> requiredField "type" "The input to send" .= fst
              <*> optionalFieldWithDefault "delay" 100 "How long to wait between keystrokes (in milliseconds)" .= snd
        )
    where
      f = \case
        Left w -> Wait w
        Right (Left s) -> SendInput s
        Right (Right (s, w)) -> Type s w
      g = \case
        Wait w -> Left w
        SendInput s -> Right (Left s)
        Type s w -> Right (Right (s, w))

commandDelay :: ASCIInemaCommand -> Word
commandDelay = \case
  Wait w -> w
  SendInput _ -> 1
  Type s i -> genericLength s * i
