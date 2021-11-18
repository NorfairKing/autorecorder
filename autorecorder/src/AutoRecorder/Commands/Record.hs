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
import Autodocodec.Yaml
import Conduit
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
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
import System.IO
import System.Process.Typed
import System.Timeout

record :: RecordSettings -> IO ()
record rs@RecordSettings {..} = do
  mSpec <- readYamlConfigFile recordSetSpecFile
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
    $ (if recordSetCleanup then withRestoredFiles asciinemaFiles else id)
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
        hSetBuffering stdout LineBuffering -- For progress output
        hSetBuffering stderr LineBuffering
        hSetBuffering tMasterHandle NoBuffering
        hSetBuffering tSlaveHandle NoBuffering
        let windowSize = deriveWindowSize rs spec
        setWindowSize tFd windowSize
        withProcessWait apc $ \p -> do
          start <- getCurrentTime
          outVar <- newTVarIO []
          let commands =
                concat
                  [ [Wait 500],
                    asciinemaInput,
                    [SendInput "exit\r" | isNothing asciinemaCommand],
                    [Wait 500]
                  ]
          let inSender = runConduit $ inputWriter specFilePath recordSetOutputView recordSetSpeed recordSetMistakes tAttributes tMasterHandle commands
          let outReader = runConduit $ outputConduit recordSetOutputView outVar tMasterHandle
          mExitedNormally <-
            timeout (asciinemaTimeout * 1000 * 1000) $
              race -- For some reason the output conduit never finishes, so this works.
                inSender
                outReader
          case mExitedNormally of
            Nothing -> do
              stopProcess p
              die $ unwords ["the recording got stuck for", show asciinemaTimeout, "seconds."]
            Just (Right _) -> die "Should not happen: The outputter finished before the inputter"
            Just (Left inputEvents) -> do
              outputEvents <- readTVarIO outVar
              ec <- waitExitCode p
              when (not asciinemaAllowFail && isNothing asciinemaExpectExitCode) $
                case ec of
                  ExitSuccess -> pure ()
                  ExitFailure c -> die $ unwords ["The casted process has exited with exit code", show c]
              forM_ asciinemaExpectExitCode $ \expected -> do
                let actual = case ec of
                      ExitSuccess -> 0
                      ExitFailure i -> fromIntegral i
                unless (actual == expected) $ die $ unwords ["The casted process has an unexpected exit code:", show actual]
              pure $ completeCast rs spec env' recordSetSpeed start inputEvents outputEvents

completeCast :: RecordSettings -> ASCIInemaSpec -> [(String, String)] -> Speed -> UTCTime -> [(UTCTime, Text)] -> [(UTCTime, ByteString)] -> Cast
completeCast sets@RecordSettings {..} spec@ASCIInemaSpec {..} env speed start inputs outputs =
  let castEvents = map (eventSpeedUp speed) $ interleaveEvents start inputs outputs
      castHeader =
        Header
          { headerWindowSize = deriveWindowSize sets spec,
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

deriveWindowSize :: RecordSettings -> ASCIInemaSpec -> WindowSize
deriveWindowSize RecordSettings {..} ASCIInemaSpec {..} =
  let windowSizeRows = fromMaybe recordSetDefaultRows $ recordSetRows <|> asciinemaRows
      windowSizeColumns = fromMaybe recordSetDefaultColumns $ recordSetColumns <|> asciinemaColumns
   in WindowSize {..}
