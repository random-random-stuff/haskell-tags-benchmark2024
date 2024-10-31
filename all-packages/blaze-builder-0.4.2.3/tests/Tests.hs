{-# LANGUAGE CPP, OverloadedStrings #-}
#if __GLASGOW_HASKELL__ >= 704
{-# OPTIONS_GHC -fsimpl-tick-factor=40000 #-}
#endif
-- | Tests for the Blaze builder
--
module Main where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Monoid (mempty, mappend, mconcat)
#endif

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.QuickCheck
import Test.HUnit hiding (Test)
import Codec.Binary.UTF8.String (decode)

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Blaze.ByteString.Builder.Html.Utf8

main :: IO ()
main = defaultMain $ return $ testGroup "Tests" tests

tests :: [Test]
tests =
    [ testProperty "left identity Monoid law"  monoidLeftIdentity
    , testProperty "right identity Monoid law" monoidRightIdentity
    , testProperty "associativity Monoid law"  monoidAssociativity
    , testProperty "mconcat Monoid law"        monoidConcat
    , testProperty "string → builder → string" fromStringToString
    , testProperty "string and text"           stringAndText
    , testProperty "lazy bytestring identity"  identityLazyByteString
    , testProperty "flushing identity"         identityFlushing
    , testProperty "writeToByteString"         writeToByteStringProp
    , testCase     "escaping case 1"           escaping1
    , testCase     "escaping case 2"           escaping2
    , testCase     "escaping case 3"           escaping3
    ]

monoidLeftIdentity :: Builder -> Bool
monoidLeftIdentity b = mappend mempty b == b

monoidRightIdentity :: Builder -> Bool
monoidRightIdentity b = mappend b mempty == b

monoidAssociativity :: Builder -> Builder -> Builder -> Bool
monoidAssociativity x y z = mappend x (mappend y z) == mappend (mappend x y) z

monoidConcat :: [Builder] -> Bool
monoidConcat xs = mconcat xs == foldr mappend mempty xs

fromStringToString :: String -> Bool
fromStringToString string = string == convert string
  where
    convert = decode . LB.unpack . toLazyByteString . fromString

stringAndText :: String -> Bool
stringAndText string = fromString string == fromText (T.pack string)

identityLazyByteString :: LB.ByteString -> Bool
identityLazyByteString lbs = lbs == toLazyByteString (fromLazyByteString lbs)

identityFlushing :: String -> String -> Bool
identityFlushing s1 s2 =
    let b1 = fromString s1
        b2 = fromString s2
    in b1 `mappend` b2 == b1 `mappend` flush `mappend` b2

writeToByteStringProp :: Write -> Bool
writeToByteStringProp w = toByteString (fromWrite w) == writeToByteString w

escaping1 :: Assertion
escaping1 = fromString "&lt;hello&gt;" @?= fromHtmlEscapedString "<hello>"

escaping2 :: Assertion
escaping2 = fromString "f &amp;&amp;&amp; g" @?= fromHtmlEscapedString "f &&& g"

escaping3 :: Assertion
escaping3 = fromString "&quot;&#39;" @?= fromHtmlEscapedString "\"'"

#if !MIN_VERSION_bytestring(0,11,1)
instance Show Builder where
    show = show . toLazyByteString
#endif

instance Show Write where
    show = show . fromWrite

instance Eq Builder where
    b1 == b2 =
        -- different and small buffer sizses for testing wrapping behaviour
        toLazyByteStringWith  1024     1024 256 b1 mempty ==
        toLazyByteStringWith  2001     511  256 b2 mempty

-- | Artificially scale up size to ensures that buffer wrapping behaviour is
-- also tested.
numRepetitions :: Int
numRepetitions = 250

instance Arbitrary Builder where
    arbitrary = (mconcat . replicate numRepetitions . fromString) <$> arbitrary

instance Arbitrary Write where
    arbitrary = mconcat . map singleWrite <$> arbitrary
      where
        singleWrite (Left bs) = writeByteString (mconcat (LB.toChunks bs))
        singleWrite (Right w) = writeWord8 w

instance Arbitrary LB.ByteString where
    arbitrary = (LB.concat . replicate numRepetitions . LB.pack) <$> arbitrary
