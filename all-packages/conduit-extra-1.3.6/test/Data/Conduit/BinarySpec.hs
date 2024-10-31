{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Conduit.BinarySpec (spec) where

import Data.Conduit (runConduit, runConduitRes, (.|), runConduitPure, ConduitT, Void)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Control.Exception (IOException)
import qualified Data.ByteString.Lazy as L
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.IORef as I
import Control.Monad.Trans.Writer.Strict
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Functor.Identity
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, oneof)
import Data.Word (Word8)
import Foreign.Storable (Storable, sizeOf, pokeByteOff, alignment)
import Data.Typeable (Typeable)
import Data.ByteString.Internal (createAndTrim')
import Foreign.Ptr (alignPtr, minusPtr)
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)
import Control.Applicative ((<$>))
import qualified Data.ByteString.Builder as BB

spec :: Spec
spec = describe "Data.Conduit.Binary" $ do

    describe "file access" $ do
        it "read" $ do
            bs <- S.readFile "conduit-extra.cabal"
            bss <- runConduitRes $ CB.sourceFile "conduit-extra.cabal" .| CL.consume
            S.concat bss `shouldBe` bs

        it "read range" $ do
            S.writeFile "tmp" "0123456789"
            bss <- runConduitRes $ CB.sourceFileRange "tmp" (Just 2) (Just 3) .| CL.consume
            S.concat bss `shouldBe` "234"

        it "write" $ do
            runConduitRes $ CB.sourceFile "conduit-extra.cabal" .| CB.sinkFile "tmp"
            bs1 <- S.readFile "conduit-extra.cabal"
            bs2 <- S.readFile "tmp"
            bs2 `shouldBe` bs1


        it "write builder (withSinkFileBuilder)" $ do
            CB.withSinkFileBuilder "tmp" $ \sink ->
              CB.withSourceFile "conduit-extra.cabal" $ \src ->
              runConduit $ src .| CL.map BB.byteString .| sink
            bs1 <- S.readFile "conduit-extra.cabal"
            bs2 <- S.readFile "tmp"
            bs2 `shouldBe` bs1

        it "conduit" $ do
            runConduitRes
                $ CB.sourceFile "conduit-extra.cabal"
               .| CB.conduitFile "tmp"
               .| CB.sinkFile "tmp2"
            bs1 <- S.readFile "conduit-extra.cabal"
            bs2 <- S.readFile "tmp"
            bs3 <- S.readFile "tmp2"
            bs2 `shouldBe` bs1
            bs3 `shouldBe` bs1
    describe "binary isolate" $ do
        it "works" $ do
            bss <- runConduitRes
                 $ CL.sourceList (replicate 1000 "X")
                .| CB.isolate 6
                .| CL.consume
            S.concat bss `shouldBe` "XXXXXX"

    describe "properly using binary file reading" $ do
        it "sourceFile" $ do
            x <- runConduitRes $ CB.sourceFile "test/random" .| CL.consume
            lbs <- L.readFile "test/random"
            L.fromChunks x `shouldBe` lbs

    describe "binary head" $ do
        let go lbs = do
                x <- CB.head
                case (x, L.uncons lbs) of
                    (Nothing, Nothing) -> return True
                    (Just y, Just (z, lbs'))
                        | y == z -> go lbs'
                    _ -> return False

        prop "works" $ \bss' ->
            let bss = map S.pack bss'
             in runConduitPure $ CL.sourceList bss .| go (L.fromChunks bss)
    describe "binary takeWhile" $ do
        prop "works" $ \bss' ->
            let bss = map S.pack bss'
             in runIdentity $ do
                bss2 <- runConduit $ CL.sourceList bss .| CB.takeWhile (>= 5) .| CL.consume
                return $ L.fromChunks bss2 == L.takeWhile (>= 5) (L.fromChunks bss)
        prop "leftovers present" $ \bss' ->
            let bss = map S.pack bss'
             in runIdentity $ do
                result <- runConduit $ CL.sourceList bss .| do
                    x <- CB.takeWhile (>= 5) .| CL.consume
                    y <- CL.consume
                    return (S.concat x, S.concat y)
                let expected = S.span (>= 5) $ S.concat bss
                if result == expected
                    then return True
                    else error $ show (S.concat bss, result, expected)

    describe "binary dropWhile" $ do
        prop "works" $ \bss' ->
            let bss = map S.pack bss'
             in runIdentity $ do
                bss2 <- runConduit $ CL.sourceList bss .| do
                    CB.dropWhile (< 5)
                    CL.consume
                return $ L.fromChunks bss2 == L.dropWhile (< 5) (L.fromChunks bss)

    describe "binary take" $ do
      let go n l = runConduit $ CL.sourceList l .| do
              a <- CB.take n
              b <- CL.consume
              return (a, b)

      -- Taking nothing should result in an empty Bytestring
      it "nothing" $ do
        (a, b) <- runResourceT $ go 0 ["abc", "defg"]
        a              `shouldBe` L.empty
        L.fromChunks b `shouldBe` "abcdefg"

      it "normal" $ do
        (a, b) <- runResourceT $ go 4 ["abc", "defg"]
        a              `shouldBe` "abcd"
        L.fromChunks b `shouldBe` "efg"

      -- Taking exactly the data that is available should result in no
      -- leftover.
      it "all" $ do
        (a, b) <- runResourceT $ go 7 ["abc", "defg"]
        a `shouldBe` "abcdefg"
        b `shouldBe` []

      -- Take as much as possible.
      it "more" $ do
        (a, b) <- runResourceT $ go 10 ["abc", "defg"]
        a `shouldBe` "abcdefg"
        b `shouldBe` []

    describe "binary" $ do
        prop "lines" $ \bss' -> runIdentity $ do
            let bss = map S.pack bss'
                bs = S.concat bss
                src = CL.sourceList bss
            res <- runConduit $ src .| CB.lines .| CL.consume
            return $ S8.lines bs == res

    describe "sinkCacheLength" $ do
        it' "works" $ runResourceT $ do
            lbs <- liftIO $ L.readFile "test/Data/Conduit/BinarySpec.hs"
            (len, src) <- runConduit $ CB.sourceLbs lbs .| CB.sinkCacheLength
            lbs' <- runConduit $ src .| CB.sinkLbs
            liftIO $ do
                fromIntegral len `shouldBe` L.length lbs
                lbs' `shouldBe` lbs
                fromIntegral len `shouldBe` L.length lbs'

    describe "sinkFileCautious" $ do
      it' "success" $ do
        runConduitRes $ CB.sourceFile "conduit-extra.cabal" .| CB.sinkFileCautious "tmp"
        bs1 <- S.readFile "conduit-extra.cabal"
        bs2 <- S.readFile "tmp"
        bs2 `shouldBe` bs1
      it' "failure" $ do
        let bs1 = "This is the original content"
        S.writeFile "tmp" bs1
        runConduitRes
               ( (CB.sourceFile "conduit-extra.cabal" >> error "FIXME")
            .| CB.sinkFileCautious "tmp")
               `shouldThrow` anyException
        bs2 <- S.readFile "tmp"
        bs2 `shouldBe` bs1

    it "sinkSystemTempFile" $ do
        let bs = "Hello World!"
        fp <- runResourceT $ do
          fp <- runConduit $ C.yield bs .| CB.sinkSystemTempFile "temp-file-test"
          liftIO $ do
            actual <- S.readFile fp
            actual `shouldBe` bs
          return fp
        exists <- doesFileExist fp
        exists `shouldBe` False

    describe "Data.Conduit.Binary.mapM_" $ do
        prop "telling works" $ \bytes ->
            let lbs = L.pack bytes
                src = CB.sourceLbs lbs
                sink = CB.mapM_ (tell . return . S.singleton)
                bss = execWriter $ runConduit $ src .| sink
             in L.fromChunks bss == lbs

    describe "exception handling" $ do
        it "catchC" $ do
            ref <- I.newIORef 0
            let src = do
                    C.catchC (CB.sourceFile "some-file-that-does-not-exist") onErr
                    C.handleC onErr $ CB.sourceFile "conduit-extra.cabal"
                onErr :: MonadIO m => IOException -> m ()
                onErr _ = liftIO $ I.modifyIORef ref (+ 1)
            contents <- L.readFile "conduit-extra.cabal"
            res <- runConduitRes $ src .| CB.sinkLbs
            res `shouldBe` contents
            errCount <- I.readIORef ref
            errCount `shouldBe` (1 :: Int)
        it "tryC" $ do
            ref <- I.newIORef undefined
            let src = do
                    res1 <- C.tryC $ CB.sourceFile "some-file-that-does-not-exist"
                    res2 <- C.tryC $ CB.sourceFile "conduit-extra.cabal"
                    liftIO $ I.writeIORef ref (res1, res2)
            contents <- L.readFile "conduit-extra.cabal"
            res <- runConduitRes $ src .| CB.sinkLbs
            res `shouldBe` contents
            exc <- I.readIORef ref
            case exc :: (Either IOException (), Either IOException ()) of
                (Left _, Right ()) ->
                    return ()
                _ -> error $ show exc

    describe "normalFuseLeft" $ do
        it "does not double close conduit" $ do
            x <- runConduitRes $
                let src = CL.sourceList ["foobarbazbin"]
                 in src .| CB.isolate 10 .| CL.head
            x `shouldBe` Just "foobarbazb"

    describe "Storable" $ do
        let test name func = describe name $ do
                let test' size =
                      prop ("chunk size " ++ show size) $ \stores0 -> do
                        let src =
                                loop (someStorables stores0)
                              where
                                loop bs
                                    | S.null bs = return ()
                                    | otherwise = do
                                        let (x, y) = S.splitAt size bs
                                        C.yield x
                                        loop y

                            sink :: [SomeStorable]
                                 -> ConduitT S.ByteString Void IO ()
                            sink [] = do
                                mw <- CB.head
                                case mw of
                                    Nothing -> return ()
                                    Just _ -> error "trailing bytes"
                            sink (next:rest) = do
                                withSomeStorable next checkOne
                                sink rest

                            checkOne :: (Storable a, Eq a, Show a)
                                     => a
                                     -> ConduitT S.ByteString Void IO ()
                            checkOne expected = do
                                mactual <-
                                    if func
                                        then CB.sinkStorable
                                        else fmap Just CB.sinkStorableEx
                                actual <-
                                    case mactual of
                                        Nothing -> error "got Nothing"
                                        Just actual -> return actual
                                liftIO $ actual `shouldBe` expected

                        runConduit $ src .| sink stores0 :: IO ()
                mapM_ test' [1, 5, 10, 100]

        test "sink Maybe" True
        test "sink exception" False

        it' "insufficient bytes are leftovers, one chunk" $ do
            let src = C.yield $ S.singleton 1
            runConduit $ src .| do
                mactual <- CB.sinkStorable
                liftIO $ mactual `shouldBe` (Nothing :: Maybe Int)
                lbs <- CB.sinkLbs
                liftIO $ lbs `shouldBe` L.singleton 1

        it' "insufficient bytes are leftovers, multiple chunks" $ do
            let src = do
                    C.yield $ S.singleton 1
                    C.yield $ S.singleton 2
            runConduit $ src .| do
                mactual <- CB.sinkStorable
                liftIO $ mactual `shouldBe` (Nothing :: Maybe Int)
                lbs <- CB.sinkLbs
                liftIO $ lbs `shouldBe` L.pack [1, 2]

data SomeStorable where
    SomeStorable :: (Storable a, Eq a, Show a, Typeable a) => a -> SomeStorable
instance Show SomeStorable where
    show (SomeStorable x) = show x
instance Arbitrary SomeStorable where
    arbitrary = oneof
        [ SomeStorable <$> (arbitrary :: Gen Int)
        , SomeStorable <$> (arbitrary :: Gen Word8)
        , SomeStorable <$> (arbitrary :: Gen Double)
        ]

withSomeStorable :: SomeStorable
                 -> (forall a. (Storable a, Eq a, Show a) => a -> b)
                 -> b
withSomeStorable (SomeStorable x) f = f x

someStorable :: SomeStorable -> S.ByteString
someStorable store =
    fst $ unsafePerformIO $ createAndTrim' (size + align) start
  where
    size = withSomeStorable store sizeOf
    align = withSomeStorable store alignment
    start ptr = do
        let off = minusPtr ptr (alignPtr ptr align)
        withSomeStorable store (pokeByteOff ptr off)
        return (off, size, ())

someStorables :: [SomeStorable] -> S.ByteString
someStorables = S.concat . map someStorable

it' :: String -> IO () -> Spec
it' = it
