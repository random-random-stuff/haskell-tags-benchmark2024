-- File created: 2009-01-30 15:01:02

{-# LANGUAGE CPP #-}

module Tests.Instances (tests) where

-- Monoid is re-exported from Prelude as of 4.8.0.0
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty, mappend)
#endif
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck (Property, (==>))

import System.FilePath.Glob.Base (Token(Unmatchable), tryCompileWith, unPattern)

import Tests.Base

tests :: Test
tests = testGroup "Instances"
   [ testProperty "monoid-law-1" prop_monoidLaw1
   , testProperty "monoid-law-2" prop_monoidLaw2
   , testProperty "monoid-law-3" prop_monoidLaw3
   , testProperty "monoid-4"     prop_monoid4
   ]

-- The monoid laws: associativity...
prop_monoidLaw1 :: COpts -> PString -> PString -> PString -> Property
prop_monoidLaw1 opt x y z =
   let o       = unCOpts opt
       es      = map (tryCompileWith o . unPS) [x,y,z]
       [a,b,c] = map fromRight es
    in all isRight es ==> mappend a (mappend b c) == mappend (mappend a b) c

-- ... left identity ...
prop_monoidLaw2 :: COpts -> PString -> Property
prop_monoidLaw2 opt x =
   let o = unCOpts opt
       e = tryCompileWith o (unPS x)
       a = fromRight e
    in isRight e ==> mappend mempty a == a

-- ... and right identity.
prop_monoidLaw3 :: COpts -> PString -> Property
prop_monoidLaw3 opt x =
   let o = unCOpts opt
       e = tryCompileWith o (unPS x)
       a = fromRight e
    in isRight e ==> mappend a mempty == a

-- mappending two Patterns should be equivalent to appending the original
-- strings they came from and compiling that
--
-- (notice: relies on the fact that our Arbitrary instance doesn't generate
-- unclosed [] or <>; we only check for **/ and Unmatchable)
prop_monoid4 :: COpts -> PString -> PString -> Property
prop_monoid4 opt x y =
   let o     = unCOpts opt
       es    = map (tryCompileWith o . unPS) [x,y]
       [a,b] = map fromRight es
       cat1  = mappend a b
       cat2  = tryCompileWith o (unPS x ++ unPS y)
       last2 = take 2 . reverse . unPS $ x
       head2 = take 2 . unPS $ y
    in     (last2 /= "**" && take 1 head2 /= "/")
        && (take 1 last2 /= "*" && take 2 head2 /= "*/")
        && all isRight es && isRight cat2
        && take 1 (unPattern b) /= [Unmatchable]
       ==> cat1 == fromRight cat2
