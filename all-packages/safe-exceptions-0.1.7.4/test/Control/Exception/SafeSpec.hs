{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Exception.SafeSpec (spec) where

import Control.Concurrent (threadDelay, newEmptyMVar, forkIOWithUnmask, takeMVar, putMVar)
import Control.Exception (ArithException (..), AsyncException (..), BlockedIndefinitelyOnMVar (..), BlockedIndefinitelyOnSTM (..))
import qualified Control.Exception as E
import Control.Exception.Safe
import Control.Monad (forever)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Typeable (Typeable)
import Data.Void (Void, absurd)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import Test.Hspec
#if !MIN_VERSION_base(4,9,0)
import System.IO.Error (isUserError)
#endif

newtype ExceptionPred = ExceptionPred { getExceptionPred :: Maybe () } deriving (Show, Eq, Typeable)

instance Exception ExceptionPred

-- | Ugly hack needed because the underlying type is not exported
timeoutException :: SomeException
timeoutException =
    case unsafePerformIO $ mask $ \restore -> timeout 1 $ tryAsync $ restore $ forever $ threadDelay maxBound of
        Nothing -> error "timeoutException returned Nothing"
        Just (Left e) -> e
        Just (Right e) -> absurd e

asyncE :: IO a
asyncE = E.throwIO ThreadKilled

syncE :: IO a
syncE = E.throwIO Overflow

-- | Maps each exception to whether it is synchronous
exceptions :: [(SomeException, Bool)]
exceptions =
    [ go Overflow True
    , go ThreadKilled False
    , go timeoutException False
    , go BlockedIndefinitelyOnMVar True -- see the README, this is weird
    , go BlockedIndefinitelyOnSTM True -- see the README, this is weird
    ]
  where
    go e b = (toException e, b)

withAll :: (SomeException -> Bool -> IO ()) -> Spec
withAll f = mapM_ (\(e, b) -> it (show e) (f e b)) exceptions

data ResourceAction
  = ResourceAcquire
  | ResourceUse
  | ResourceRelease
  | ExceptionObserve ExceptionPred
  deriving (Show, Eq)

spec :: Spec
spec = do
    describe "isSyncException" $ withAll
        $ \e sync -> isSyncException e `shouldBe` sync
    describe "isAsncException" $ withAll
        $ \e sync -> isAsyncException e `shouldBe` not sync
    describe "toSyncException" $ withAll
        $ \e _ -> isSyncException (toSyncException e) `shouldBe` True
    describe "toAsyncException" $ withAll
        $ \e _ -> isAsyncException (toAsyncException e) `shouldBe` True

    let shouldBeSync :: Either SomeException Void -> IO ()
        shouldBeSync (Left e)
            | isSyncException e = return ()
            | otherwise = error $ "Unexpected async exception: " ++ show e
        shouldBeSync (Right x) = absurd x

        shouldBeAsync :: Either SomeException Void -> IO ()
        shouldBeAsync (Left e)
            | isAsyncException e = return ()
            | otherwise = error $ "Unexpected sync exception: " ++ show e
        shouldBeAsync (Right x) = absurd x

        shouldThrowSync f = E.try f >>= shouldBeSync
        shouldThrowAsync f = E.try f >>= shouldBeAsync

    describe "throw" $ withAll $ \e _ -> shouldThrowSync (throw e)
    describe "throwTo" $ withAll $ \e _ -> do
        var <- newEmptyMVar
        tid <- E.uninterruptibleMask_ $ forkIOWithUnmask $ \restore -> do
            res <- E.try $ restore $ forever $ threadDelay maxBound
            putMVar var res
        throwTo tid e
        res <- takeMVar var
        shouldBeAsync res

    describe "stays async" $ do
        let withPairs f = do
                it "sync/sync" $ shouldThrowSync $ f syncE syncE

                -- removing this case from consideration, since cleanup handlers
                -- cannot receive async exceptions. See
                -- https://github.com/fpco/safe-exceptions/issues/2
                --
                -- it "sync/async" $ shouldThrowAsync $ f syncE asyncE

                it "async/sync" $ shouldThrowAsync $ f asyncE syncE
                it "async/async" $ shouldThrowAsync $ f asyncE asyncE
        describe "onException" $ withPairs $ \e1 e2 -> e1 `onException` e2
        describe "withException" $ withPairs $ \e1 e2 -> e1 `withException` (\(_ :: SomeException) -> e2)
        describe "bracket_" $ withPairs $ \e1 e2 -> bracket_ (return ()) e2 e1
        describe "finally" $ withPairs $ \e1 e2 -> e1 `finally` e2
        describe "bracketOnError_" $ withPairs $ \e1 e2 -> bracketOnError_ (return ()) e2 e1

    describe "deepseq" $ do
        describe "catchAnyDeep" $ withAll $ \e _ -> do
            res <- return (impureThrow e) `catchAnyDeep` \_ -> return ()
            res `shouldBe` ()
        describe "handleAnyDeep" $ withAll $ \e _ -> do
            res <- handleAnyDeep (const $ return ()) (return (impureThrow e))
            res `shouldBe` ()
        describe "tryAnyDeep" $ withAll $ \e _ -> do
            res <- tryAnyDeep (return (impureThrow e))
            -- deal with a missing NFData instance
            shouldBeSync $ either Left (\() -> Right undefined) res
        describe "catchesDeep" $ withAll $ \e _ -> do
            res <- return (impureThrow e) `catchesDeep` [Handler (\(_ :: SomeException) -> return ())]
            res `shouldBe` ()

    describe "catchJust" $ do
      it "catches a selected exception" $ do
        res <- catchJust getExceptionPred (throw (ExceptionPred (Just ()))) (return . Just)
        res `shouldBe` Just ()

      it "re-raises a selection that is passed on" $ do
        let ex = ExceptionPred Nothing
        res <- try (catchJust getExceptionPred (throw ex) (return . Just))
        res `shouldBe` Left ex

    describe "throwString" $ do
      it "is a StringException" $
        throwString "foo" `catch` \(StringException _ _) -> return () :: IO ()

    describe "bracketWithError" $ do
      it "should prioritize exceptions from thing" $ do
        actionLogRef <- newIORef []
        eiResult <-
          try $
            Control.Exception.Safe.bracketWithError
              ( do
                  modifyIORef actionLogRef (ResourceAcquire :)
              )
              ( \mbEx () -> do
                  case mbEx of
                      Just ex | Just exPred <- fromException ex ->
                          modifyIORef actionLogRef (ExceptionObserve exPred :)
                      _ -> pure ()
                  modifyIORef actionLogRef (ResourceRelease :)
                  throw $ ExceptionPred $ Just ()
              )
              ( \() -> do
                  modifyIORef actionLogRef (ResourceUse :)
                  throw $ ExceptionPred Nothing
                  pure ()
              )
        eiResult `shouldBe` Left (ExceptionPred Nothing)
        readIORef actionLogRef
          `shouldReturn` [ResourceRelease, ExceptionObserve (ExceptionPred Nothing), ResourceUse, ResourceAcquire]

      it "should lift through ExceptT" $ do
        actionLogRef <- newIORef []
        eiResult <-
          runExceptT $
            Control.Exception.Safe.bracketWithError
              ( do
                  lift $ modifyIORef actionLogRef (ResourceAcquire :)
              )
              ( \_ () -> do
                  lift $ modifyIORef actionLogRef (ResourceRelease :)
              )
              ( \() -> do
                  lift $ modifyIORef actionLogRef (ResourceUse :)
                  throwE $ ExceptionPred Nothing
                  pure ()
              )
        eiResult `shouldBe` Left (ExceptionPred Nothing)
        readIORef actionLogRef
          `shouldReturn` [ResourceRelease, ResourceUse, ResourceAcquire]

