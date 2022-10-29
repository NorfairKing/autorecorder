{-# LANGUAGE OverloadedStrings #-}

module AutoRecorder.Output where

import Conduit
import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.Conduit.Combinators as C
import Data.Time
import GHC.IO.Handle

data OutputView
  = NoOutputView
  | DebugOutputView
  | ProgressOutputView
  | DisplayOutputView
  deriving (Show, Eq)

outputConduit :: MonadIO m => OutputView -> TVar [(UTCTime, ByteString)] -> Handle -> ConduitT () void m ()
outputConduit ov outVar h =
  sourceHandle h
    .| ( case ov of
           DisplayOutputView -> (outputDisplayConduit .|)
           DebugOutputView -> (outputDebugConduit .|)
           _ -> id
       )
      outputTimerConduit
    .| outputSink outVar

outputSink :: MonadIO m => TVar [(UTCTime, ByteString)] -> ConduitT (UTCTime, ByteString) void m ()
outputSink outVar = awaitForever $ \t -> liftIO $ atomically $ modifyTVar' outVar (t :)

outputTimerConduit :: MonadIO m => ConduitT i (UTCTime, i) m ()
outputTimerConduit = C.mapM $ \i -> (,) <$> liftIO getCurrentTime <*> pure i

outputDebugConduit :: MonadIO m => ConduitT ByteString ByteString m ()
outputDebugConduit = C.mapM $ \bs -> do
  liftIO $ putStrLn $ "Got output: " <> show bs
  pure bs

outputDisplayConduit :: MonadIO m => ConduitT ByteString ByteString m ()
outputDisplayConduit = C.mapM $ \bs -> do
  liftIO $ SB.putStr bs
  pure bs
