-- |
-- Copyright:   (c) 2021-2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module MixedRope
  ( testSuite
  ) where

import Prelude ((+), (-))
import Data.Bool (Bool(..), (&&))
import Data.Function (($))
import Data.Maybe (Maybe(..), isJust)
import Data.Semigroup ((<>))
import qualified Data.Text.Lines as Char
import qualified Data.Text.Utf16.Lines as Utf16
import qualified Data.Text.Utf16.Rope.Mixed as Mixed
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty, (===), property, (.&&.), counterexample)

import Utils ()

testSuite :: TestTree
testSuite = testGroup "Utf16 Mixed"
  [ testProperty "char null" $
    \x -> Mixed.null x === Char.null (Mixed.toTextLines x)

  , testProperty "charLength" $
    \x -> Mixed.charLength x === Char.length (Mixed.toTextLines x)
  , testProperty "utf16Length" $
    \x -> Mixed.utf16Length x === Utf16.length (Mixed.toTextLines x)

  , testProperty "lengthInLines" $
    \x -> Mixed.lengthInLines x === Char.lengthInLines (Mixed.toTextLines x)

  , testProperty "lines" $
    \x -> Mixed.lines x === Char.lines (Mixed.toTextLines x)

  , testProperty "splitAtLine" $
    \i x -> let (y, z) = Mixed.splitAtLine i x in
      (Mixed.toTextLines y, Mixed.toTextLines z) === Char.splitAtLine i (Mixed.toTextLines x)

  , testProperty "charSplitAt 1" $
    \i x -> case Mixed.charSplitAt i x of
      (y, z) -> x === y <> z
  , testProperty "charSplitAt 2" $
    \i x -> case (Mixed.charSplitAt i x, Char.splitAt i (Char.fromText $ Mixed.toText x)) of
      ((y, z), (y', z')) -> Char.fromText (Mixed.toText y) === y' .&&. Char.fromText (Mixed.toText z) === z'

  , testProperty "utf16SplitAt 1" $
    \i x -> case Mixed.utf16SplitAt i x of
      Nothing -> property True
      Just (y, z) -> x === y <> z
  , testProperty "utf16SplitAt 2" $
    \i x -> case (Mixed.utf16SplitAt i x, Utf16.splitAt i (Utf16.fromText $ Mixed.toText x)) of
      (Nothing, Nothing) -> property True
      (Nothing, Just{}) -> counterexample "can split TextLines, but not Mixed" False
      (Just{}, Nothing) -> counterexample "can split Mixed, but not TextLines" False
      (Just (y, z), Just (y', z')) -> Utf16.fromText (Mixed.toText y) === y' .&&. Utf16.fromText (Mixed.toText z) === z'
  , testProperty "splitAt 3" $
    \i x -> case Mixed.utf16SplitAt i x of
      Just{} -> True
      Nothing -> isJust (Mixed.utf16SplitAt (i - 1) x) && isJust (Mixed.utf16SplitAt (i + 1) x)

  , testProperty "charSplitAtPosition 1" $
    \i x -> case Mixed.charSplitAtPosition i x of
      (y, z) -> x === y <> z
  , testProperty "charSplitAtPosition 2" $
    \i x -> case (Mixed.charSplitAtPosition i x, Char.splitAtPosition i (Char.fromText $ Mixed.toText x)) of
      ((y, z), (y', z')) -> Char.fromText (Mixed.toText y) === y' .&&. Char.fromText (Mixed.toText z) === z'

  , testProperty "utf16SplitAtPosition 1" $
    \i x -> case Mixed.utf16SplitAtPosition i x of
      Nothing -> property True
      Just (y, z) -> x === y <> z
  , testProperty "utf16SplitAtPosition 2" $
    \i x -> case (Mixed.utf16SplitAtPosition i x, Utf16.splitAtPosition i (Utf16.fromText $ Mixed.toText x)) of
      (Nothing, Nothing) -> property True
      (Nothing, Just{}) -> counterexample "can split TextLines, but not Mixed" False
      (Just{}, Nothing) -> counterexample "can split Mixed, but not TextLines" False
      (Just (y, z), Just (y', z')) -> Utf16.fromText (Mixed.toText y) === y' .&&. Utf16.fromText (Mixed.toText z) === z'
  , testProperty "utf16SplitAtPosition 3" $
    \i x -> case Mixed.utf16SplitAtPosition i x of
      Just{} -> True
      Nothing -> isJust (Mixed.utf16SplitAtPosition (i <> Utf16.Position 0 1) x)
  ]
