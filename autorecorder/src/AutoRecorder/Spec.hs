{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AutoRecorder.Spec where

import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Yaml
import YamlParse.Applicative

data ASCIInemaSpec
  = ASCIInemaSpec
      { asciinemaCommand :: Maybe String,
        asciinemaTimeout :: Int, -- Seconds
        asciinemaFiles :: [FilePath],
        asciinemaWorkingDir :: Maybe FilePath,
        asciinemaEnvironment :: Map String String,
        asciinemaInput :: [ASCIInemaCommand]
      }
  deriving (Show, Eq)

instance FromJSON ASCIInemaSpec where
  parseJSON = viaYamlSchema

instance YamlSchema ASCIInemaSpec where
  yamlSchema =
    objectParser "ASCIInemaSpec" $
      ASCIInemaSpec
        <$> optionalField "command" "The command to show off. Leave this to just run a shell"
        <*> optionalFieldWithDefault "timeout" 60 "How long to allow the recording to run before timing out, in seconds"
        <*> alternatives
          [ (: []) <$> requiredField "file" "The file that is being touched. It will be brought back in order afterwards.",
            optionalFieldWithDefault "files" [] "The files that are being touched. These will be brought back in order afterwards."
          ]
        <*> optionalField "working-dir" "The working directory directory"
        <*> optionalFieldWithDefault "environment" M.empty "Variables to add to the environment"
        <*> optionalFieldWithDefault "input" [] "The inputs to send to the command"

data ASCIInemaCommand
  = Wait Word -- Milliseconds
  | SendInput String
  | Type String Word -- Milliseconds
  deriving (Show, Eq)

instance FromJSON ASCIInemaCommand where
  parseJSON = viaYamlSchema

instance YamlSchema ASCIInemaCommand where
  yamlSchema =
    alternatives
      [ objectParser "Wait" $ Wait <$> requiredField "wait" "How long to wait (in milliseconds)",
        objectParser "SendInput" $ SendInput <$> requiredField "send" "The input to send",
        objectParser "Type" $
          Type
            <$> requiredField "type" "The input to send"
            <*> optionalFieldWithDefault "delay" 100 "How long to wait between keystrokes (in milliseconds)"
      ]

commandDelay :: ASCIInemaCommand -> Word
commandDelay = \case
  Wait w -> w
  SendInput _ -> 1
  Type s i -> genericLength s * i
