{-# LANGUAGE DeriveDataTypeable #-}

-- | The underlying CmdArgs type.
module System.Console.CmdArgs.Implicit.Type(
    -- cmdArgs_privateArgsSeen is exported, otherwise Haddock
    -- gets confused when using RecordWildCards
    CmdArgs(..), cmdArgsHasValue, embed, reembed,
    CmdArgsPrivate, incArgsSeen, getArgsSeen
    ) where

import System.Console.CmdArgs.Verbosity

import Data.Data
import Data.Maybe


-- | A structure to store the additional data relating to @--help@,
--   @--version@, @--quiet@ and @--verbose@.
data CmdArgs a = CmdArgs
    {cmdArgsValue :: a -- ^ The underlying value being wrapped.
    ,cmdArgsHelp :: Maybe String -- ^ @Just@ if @--help@ is given, then gives the help message for display, including a trailing newline.
    ,cmdArgsVersion :: Maybe String -- ^ @Just@ if @--version@ is given, then gives the version message for display, including a trailing newline.
    ,cmdArgsVerbosity :: Maybe Verbosity -- ^ @Just@ if @--quiet@ or @--verbose@ is given, then gives the verbosity to use.
    ,cmdArgsPrivate :: CmdArgsPrivate -- ^ Private: Only exported due to Haddock limitations.
    }
    deriving (Show,Eq,Ord,Data,Typeable)

cmdArgsHasValue :: CmdArgs a -> Bool
cmdArgsHasValue x = isNothing (cmdArgsHelp x) && isNothing (cmdArgsVersion x)

instance Functor CmdArgs where
    fmap f x = x{cmdArgsValue = f $ cmdArgsValue x}


embed :: a -> CmdArgs a
embed x = CmdArgs x Nothing Nothing Nothing (CmdArgsPrivate 0)

reembed :: CmdArgs a -> (a, a -> CmdArgs a)
reembed x = (cmdArgsValue x, \y -> x{cmdArgsValue=y})


data CmdArgsPrivate = CmdArgsPrivate
    Int -- ^ The number of arguments that have been seen
    deriving (Eq,Ord,Data,Typeable)

incArgsSeen x@CmdArgs{cmdArgsPrivate = CmdArgsPrivate i} = x{cmdArgsPrivate = CmdArgsPrivate (i+1)}
getArgsSeen CmdArgs{cmdArgsPrivate = CmdArgsPrivate i} = i

instance Show CmdArgsPrivate where show _ = "CmdArgsPrivate"
