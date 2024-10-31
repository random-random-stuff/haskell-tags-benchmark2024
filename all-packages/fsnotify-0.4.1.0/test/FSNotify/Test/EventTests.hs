{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant multi-way if" #-}

module FSNotify.Test.EventTests where

import Control.Exception.Safe (MonadThrow)
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import FSNotify.Test.Util
import Prelude hiding (FilePath)
import System.FSNotify
import System.FilePath
import System.IO (hPutStr)
import Test.Sandwich
import UnliftIO hiding (poll)
import UnliftIO.Directory


eventTests :: (MonadUnliftIO m, MonadThrow m, HasParallelSemaphore' context) => ThreadingMode -> SpecFree context m ()
eventTests threadingMode = describe "Tests" $ parallel $ do
  let pollOptions = if isBSD then [True] else [False, True]

  forM_ pollOptions $ \poll -> describe (if poll then "Polling" else "Native") $ parallel $ do
    let ?timeInterval = if poll then 2*10^(6 :: Int) else 5*10^(5 :: Int)
    forM_ [False, True] $ \recursive -> describe (if recursive then "Recursive" else "Non-recursive") $ parallel $
      forM_ [False, True] $ \nested -> describe (if nested then "Nested" else "Non-nested") $ parallel $
        eventTests' threadingMode poll recursive nested



eventTests' :: (MonadUnliftIO m, MonadThrow m, HasParallelSemaphore' context, ?timeInterval :: Int) => ThreadingMode -> Bool -> Bool -> Bool -> SpecFree context m ()
eventTests' threadingMode poll recursive nested = do -- withParallelSemaphore $
  let itWithFolder name action = introduceTestFolder threadingMode poll recursive nested $ it name action

  unless (nested || poll || isMac || isWin) $ itWithFolder "deletes the watched directory" $ do
    TestFolderContext watchedDir _f getEvents _clearEvents <- getContext testFolderContext
    removeDirectory watchedDir

    pauseAndRetryOnExpectationFailure 3 $ liftIO getEvents >>= \case
      [WatchedDirectoryRemoved {..}] | eventPath `equalFilePath` watchedDir && eventIsDirectory == IsDirectory -> return ()
      events -> expectationFailure $ "Got wrong events: " <> show events

  itWithFolder "works with a new file" $ do
    TestFolderContext _watchedDir f getEvents _clearEvents <- getContext testFolderContext

    let wrapper action = if | isWin -> liftIO (writeFile f "foo") >> action
                            | otherwise -> withFile f AppendMode $ \_ -> action

    wrapper $
      pauseAndRetryOnExpectationFailure 3 $ liftIO getEvents >>= \events ->
        if | nested && not recursive -> events `shouldBe` []
           | isWin && not poll -> case events of
               [Modified {}, Added {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
               _ -> expectationFailure $ "Got wrong events: " <> show events
           | otherwise -> case events of
               [Added {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
               _ -> expectationFailure $ "Got wrong events: " <> show events

  itWithFolder "works with a new directory" $ do
    TestFolderContext _watchedDir f getEvents _clearEvents <- getContext testFolderContext
    createDirectory f

    pauseAndRetryOnExpectationFailure 3 $ liftIO getEvents >>= \events ->
      if | nested && not recursive -> events `shouldBe` []
         | otherwise -> case events of
             [Added {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsDirectory -> return ()
             _ -> expectationFailure $ "Got wrong events: " <> show events

  itWithFolder "works with a deleted file" $ do
    TestFolderContext _watchedDir f getEvents clearEvents <- getContext testFolderContext
    liftIO (writeFile f "" >> clearEvents)

    removeFile f

    pauseAndRetryOnExpectationFailure 3 $ liftIO getEvents >>= \events ->
      if | nested && not recursive -> events `shouldBe` []
         | otherwise -> case events of
             [Removed {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
             _ -> expectationFailure $ "Got wrong events: " <> show events

  itWithFolder "works with a deleted directory" $ do
    TestFolderContext _watchedDir f getEvents clearEvents <- getContext testFolderContext
    createDirectory f >> liftIO clearEvents

    removeDirectory f

    pauseAndRetryOnExpectationFailure 3 $ liftIO getEvents >>= \events ->
      if | nested && not recursive -> events `shouldBe` []
         | otherwise -> case events of
             [Removed {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsDirectory -> return ()
             _ -> expectationFailure $ "Got wrong events: " <> show events

  itWithFolder "works with modified file attributes" $ do
    TestFolderContext _watchedDir f getEvents clearEvents <- getContext testFolderContext
    liftIO (writeFile f "" >> clearEvents)

    liftIO $ changeFileAttributes f

    -- This test is disabled when polling because the PollManager only keeps track of
    -- modification time, so it won't catch an unrelated file attribute change
    pauseAndRetryOnExpectationFailure 3 $ liftIO getEvents >>= \events ->
      if | poll -> return ()
         | nested && not recursive -> events `shouldBe` []
         | isWin -> case events of
             [Modified {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
             _ -> expectationFailure $ "Got wrong events: " <> show events
         | otherwise -> case events of
             [ModifiedAttributes {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
             _ -> expectationFailure $ "Got wrong events: " <> show events

  itWithFolder "works with a modified file" $ do
    TestFolderContext _watchedDir f getEvents clearEvents <- getContext testFolderContext
    liftIO (writeFile f "" >> clearEvents)

    (if isWin then withSingleWriteFile f "foo" else withOpenWritableAndWrite f "foo") $
      pauseAndRetryOnExpectationFailure 3 $ liftIO getEvents >>= \events ->
        if | nested && not recursive -> events `shouldBe` []
           | isMac -> case events of
               [Modified {..}] | poll && eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
               [ModifiedAttributes {..}] | not poll && eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
               _ -> expectationFailure $ "Got wrong events: " <> show events <> " (wanted file path " <> show f <> ")"
           | otherwise -> case events of
               [Modified {..}] | eventPath `equalFilePath` f && eventIsDirectory == IsFile -> return ()
               _ -> expectationFailure $ "Got wrong events: " <> show events <> " (wanted file path " <> show f <> ")"

  when isLinux $ unless poll $
    itWithFolder "gets a close_write" $ do
      TestFolderContext _watchedDir f getEvents clearEvents <- getContext testFolderContext
      liftIO (writeFile f "" >> clearEvents)
      liftIO $ withFile f WriteMode $ flip hPutStr "asdf"
      pauseAndRetryOnExpectationFailure 3 $ liftIO getEvents >>= \events ->
        if | nested && not recursive -> events `shouldBe` []
           | otherwise -> case events of
               [cw@(CloseWrite {}), m@(Modified {})]
                 | eventPath cw `equalFilePath` f && eventIsDirectory cw == IsFile
                   && eventPath m `equalFilePath` f && eventIsDirectory m == IsFile -> return ()
               [m@(Modified {}), cw@(CloseWrite {})]
                 | eventPath cw `equalFilePath` f && eventIsDirectory cw == IsFile
                   && eventPath m `equalFilePath` f && eventIsDirectory m == IsFile -> return ()
               _ -> expectationFailure $ "Got wrong events: " <> show events

withSingleWriteFile :: MonadIO m => FilePath -> String -> m b -> m b
withSingleWriteFile fp contents action = do
  liftIO $ writeFile fp contents
  action

withOpenWritableAndWrite :: MonadUnliftIO m => FilePath -> String -> m b -> m b
withOpenWritableAndWrite fp contents action = do
  withFile fp WriteMode $ \h ->
    flip finally (hClose h) $ do
      liftIO $ hPutStr h contents
      action
