{-# LANGUAGE CPP #-}
module Test.Hspec.Core.Compat (
  module Imports
, module Test.Hspec.Core.Compat
) where

import           Control.Exception as Imports
import           Control.Arrow as Imports ((>>>), (&&&), first, second)
import           Control.Applicative as Imports
import           Control.Monad as Imports hiding (
    mapM
  , mapM_
  , forM
  , forM_
  , msum
  , sequence
  , sequence_
  )
import           Data.Maybe as Imports
import           Data.Foldable as Imports
import           GHC.Stack as Imports (HasCallStack, withFrozenCallStack)

import           System.IO
import           System.Exit
import           System.Environment

#if MIN_VERSION_base(4,11,0)
import           Data.Functor as Imports ((<&>))
#endif

import           Data.Traversable as Imports
import           Data.Monoid as Imports
import           Data.List as Imports (
    stripPrefix
  , isPrefixOf
  , isInfixOf
  , isSuffixOf
  , intercalate
  , inits
  , tails
  , sortBy
  , sortOn
  )

import           Prelude as Imports hiding (
    all
  , and
  , any
  , concat
  , concatMap
  , elem
  , foldl
  , foldl1
  , foldr
  , foldr1
  , mapM
  , mapM_
  , maximum
  , minimum
  , notElem
  , or
  , product
  , sequence
  , sequence_
  , sum
  )

import           Data.Typeable (Typeable, typeOf, typeRepTyCon, tyConModule, tyConName)
import           Data.IORef as Imports

#if MIN_VERSION_base(4,12,0)
import           GHC.ResponseFile as Imports (unescapeArgs)
#else
import           Data.Char
#endif

import           Text.Read as Imports (readMaybe)
import           System.Environment as Imports (lookupEnv)


import           Data.Bool as Imports (bool)

import           Control.Concurrent

showType :: Typeable a => a -> String
showType a = let t = typeRepTyCon (typeOf a) in
  show t

showFullType :: Typeable a => a -> String
showFullType a = let t = typeRepTyCon (typeOf a) in
  tyConModule t ++ "." ++ tyConName t

getDefaultConcurrentJobs :: IO Int
getDefaultConcurrentJobs = getNumCapabilities

guarded :: Alternative m => (a -> Bool) -> a -> m a
guarded p a = if p a then pure a else empty

#if !MIN_VERSION_base(4,11,0)
infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
#endif

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith = flip isSuffixOf

pass :: Applicative m => m ()
pass = pure ()

die :: String -> IO a
die err = do
  name <- getProgName
  hPutStrLn stderr $ name <> ": " <> err
  exitFailure

#if !MIN_VERSION_base(4,12,0)
unescapeArgs :: String -> [String]
unescapeArgs = filter (not . null) . unescape

data Quoting = NoneQ | SngQ | DblQ

unescape :: String -> [String]
unescape args = reverse . map reverse $ go args NoneQ False [] []
    where
      -- n.b., the order of these cases matters; these are cribbed from gcc
      -- case 1: end of input
      go []     _q    _bs   a as = a:as
      -- case 2: back-slash escape in progress
      go (c:cs) q     True  a as = go cs q     False (c:a) as
      -- case 3: no back-slash escape in progress, but got a back-slash
      go (c:cs) q     False a as
        | '\\' == c              = go cs q     True  a     as
      -- case 4: single-quote escaping in progress
      go (c:cs) SngQ  False a as
        | '\'' == c              = go cs NoneQ False a     as
        | otherwise              = go cs SngQ  False (c:a) as
      -- case 5: double-quote escaping in progress
      go (c:cs) DblQ  False a as
        | '"' == c               = go cs NoneQ False a     as
        | otherwise              = go cs DblQ  False (c:a) as
      -- case 6: no escaping is in progress
      go (c:cs) NoneQ False a as
        | isSpace c              = go cs NoneQ False []    (a:as)
        | '\'' == c              = go cs SngQ  False a     as
        | '"'  == c              = go cs DblQ  False a     as
        | otherwise              = go cs NoneQ False (c:a) as
#endif
