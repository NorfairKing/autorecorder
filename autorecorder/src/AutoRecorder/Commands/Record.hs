{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AutoRecorder.Commands.Record where

import AutoRecorder.Cast
import AutoRecorder.Input
import AutoRecorder.OptParse.Types
import AutoRecorder.Output
import AutoRecorder.Spec
import AutoRecorder.Terminal
import AutoRecorder.WindowSize
import Conduit
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import qualified Data.ByteString as SB
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.DirForest (DirForest)
import qualified Data.DirForest as DF
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time
import GHC.IO.Handle
import Path
import Path.IO
import qualified System.Directory as FP
import System.Environment (getEnvironment)
import System.Exit
import System.Process.Typed
import System.Timeout
import YamlParse.Applicative

record :: RecordSettings -> IO ()
record rs@RecordSettings {..} = do
  mSpec <- readConfigFile recordSetSpecFile
  case mSpec of
    Nothing -> die $ "File does not exist: " <> fromAbsFile recordSetSpecFile
    Just s -> do
      cast <- runASCIInema rs recordSetSpecFile s
      LB.writeFile (fromAbsFile recordSetOutputFile) (renderCast cast)

withRestoredFiles :: [FilePath] -> IO a -> IO a
withRestoredFiles fs func =
  bracket
    (getFileStati fs)
    restoreFiles
    $ const func

data FileStatus
  = DoesNotExist FilePath
  | FileWithContents (Path Abs File) ByteString
  | DirWithContents (Path Abs Dir) (DirForest ByteString)
  deriving (Show, Eq, Ord)

getFileStati :: [FilePath] -> IO (Set FileStatus)
getFileStati fs = S.fromList <$> mapM getFileStatus fs

getFileStatus :: FilePath -> IO FileStatus
getFileStatus p = do
  fileExists <- FP.doesFileExist p
  if fileExists
    then do
      fp <- resolveFile' p
      FileWithContents fp <$> SB.readFile (fromAbsFile fp)
    else do
      dirExists <- FP.doesDirectoryExist p
      if dirExists
        then do
          dp <- resolveDir' p
          DirWithContents dp <$> DF.read dp (SB.readFile . fromAbsFile)
        else pure (DoesNotExist p)

restoreFiles :: Set FileStatus -> IO ()
restoreFiles = mapM_ restoreFile . S.toList

restoreFile :: FileStatus -> IO ()
restoreFile = \case
  DoesNotExist p -> ignoringAbsence $ FP.removePathForcibly p
  FileWithContents p bs -> do
    ensureDir $ parent p
    SB.writeFile (fromAbsFile p) bs
  DirWithContents p df -> do
    ensureDir p
    DF.write p df (\p_ bs -> SB.writeFile (fromAbsFile p_) bs)

runASCIInema :: RecordSettings -> Path Abs File -> ASCIInemaSpec -> IO Cast
runASCIInema rs@RecordSettings {..} specFilePath spec@ASCIInemaSpec {..} = do
  let parentDir = parent specFilePath
  mWorkingDir <- (recordSetWorkingDir <|>) <$> mapM (resolveDir parentDir) asciinemaWorkingDir
  let dirToResolveFiles = fromMaybe parentDir mWorkingDir
  withCurrentDir dirToResolveFiles
    $ withRestoredFiles asciinemaFiles
    $ do
      -- Get the output file's parent directory ready
      env <- getEnvironment
      let env' =
            concat
              [ env,
                M.toList asciinemaEnvironment
              ]
      pc <- case asciinemaCommand of
        Nothing ->
          case lookup "SHELL" env of
            Nothing -> die "No shell configured"
            Just s -> pure $ shell s
        Just c -> pure $ shell c
      -- Make sure the output file can be created nicely
      ensureDir $ parent recordSetOutputFile
      withPseudoTerminal $ \Terminal {..} -> do
        let apc =
              maybe id (setWorkingDir . fromAbsDir) mWorkingDir
                $ setEnv env'
                $ setCreateGroup True
                $ setNewSession True
                $ setStdin (useHandleClose tSlaveHandle)
                $ setStdout (useHandleClose tSlaveHandle)
                $ setStderr (useHandleClose tSlaveHandle) pc
        hSetBuffering tMasterHandle NoBuffering
        hSetBuffering tSlaveHandle NoBuffering
        setWindowSize tFd (recordSetColumns, recordSetRows)
        withProcessWait apc $ \p -> do
          start <- getCurrentTime
          outVar <- newTVarIO []
          mExitedNormally <- timeout (asciinemaTimeout * 1000 * 1000) $ do
            let commands =
                  concat
                    [ [Wait 500],
                      asciinemaInput,
                      [SendInput "exit\r" | isNothing asciinemaCommand],
                      [Wait 500]
                    ]
            race -- For some reason the output conduit never finishes, so this works.
              (runConduit $ inputWriter recordSetOutputView recordSetSpeed recordSetMistakes tAttributes tMasterHandle commands)
              (runConduit $ outputConduit recordSetOutputView outVar tMasterHandle)
          case mExitedNormally of
            Nothing -> do
              stopProcess p
              die $ unwords ["the recording got stuck for", show asciinemaTimeout, "seconds."]
            Just (Right _) -> die "Should not happen: The outputter finished before the inputter"
            Just (Left inputEvents) -> do
              outputEvents <- readTVarIO outVar
              pure $ completeCast rs spec env' recordSetSpeed start inputEvents outputEvents

completeCast :: RecordSettings -> ASCIInemaSpec -> [(String, String)] -> Speed -> UTCTime -> [(UTCTime, Text)] -> [(UTCTime, ByteString)] -> Cast
completeCast RecordSettings {..} ASCIInemaSpec {..} env speed start inputs outputs =
  let castEvents = map (eventSpeedUp speed) $ interleaveEvents start inputs outputs
      castHeader =
        Header
          { headerWidth = recordSetColumns,
            headerHeight = recordSetRows,
            headerStartTimestamp = Just start,
            headerDuration = case sortOn Down $ map eventTime castEvents of
              [] -> Nothing
              (t : _) -> Just t,
            headerIdleTimeLimit = Nothing,
            headerCommand = asciinemaCommand,
            headerTitle = Nothing,
            headerEnv = Just $ M.filterWithKey (\k _ -> k == "TERM" || k == "SHELL") $ M.fromList env
          }
   in Cast {..}
