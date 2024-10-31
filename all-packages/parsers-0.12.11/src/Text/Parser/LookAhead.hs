{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
#define USE_DEFAULT_SIGNATURES
#endif

#ifdef USE_DEFAULT_SIGNATURES
{-# LANGUAGE DefaultSignatures, TypeFamilies #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parser.LookAhead
-- Copyright   :  (c) Edward Kmett 2011-2013
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Parsers that can 'lookAhead'.
-----------------------------------------------------------------------------
module Text.Parser.LookAhead
  (
  -- * Parsing Combinators
    LookAheadParsing(..)
  ) where

import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.Parser.Combinators

#ifdef MIN_VERSION_parsec
import qualified Text.Parsec as Parsec
#endif

#ifdef MIN_VERSION_attoparsec
import qualified Data.Attoparsec.Types as Att
import qualified Data.Attoparsec.Combinator as Att
#endif

#ifdef MIN_VERSION_binary
import qualified Data.Binary.Get as B
#endif

-- | Additional functionality needed to describe parsers independent of input type.
class Parsing m => LookAheadParsing m where
  -- | @lookAhead p@ parses @p@ without consuming any input.
  lookAhead :: m a -> m a

instance (LookAheadParsing m, MonadPlus m) => LookAheadParsing (Lazy.StateT s m) where
  lookAhead (Lazy.StateT m) = Lazy.StateT $ lookAhead . m
  {-# INLINE lookAhead #-}

instance (LookAheadParsing m, MonadPlus m) => LookAheadParsing (Strict.StateT s m) where
  lookAhead (Strict.StateT m) = Strict.StateT $ lookAhead . m
  {-# INLINE lookAhead #-}

instance (LookAheadParsing m, MonadPlus m) => LookAheadParsing (ReaderT e m) where
  lookAhead (ReaderT m) = ReaderT $ lookAhead . m
  {-# INLINE lookAhead #-}

instance (LookAheadParsing m, MonadPlus m, Monoid w) => LookAheadParsing (Strict.WriterT w m) where
  lookAhead (Strict.WriterT m) = Strict.WriterT $ lookAhead m
  {-# INLINE lookAhead #-}

instance (LookAheadParsing m, MonadPlus m, Monoid w) => LookAheadParsing (Lazy.WriterT w m) where
  lookAhead (Lazy.WriterT m) = Lazy.WriterT $ lookAhead m
  {-# INLINE lookAhead #-}

instance (LookAheadParsing m, MonadPlus m, Monoid w) => LookAheadParsing (Lazy.RWST r w s m) where
  lookAhead (Lazy.RWST m) = Lazy.RWST $ \r s -> lookAhead (m r s)
  {-# INLINE lookAhead #-}

instance (LookAheadParsing m, MonadPlus m, Monoid w) => LookAheadParsing (Strict.RWST r w s m) where
  lookAhead (Strict.RWST m) = Strict.RWST $ \r s -> lookAhead (m r s)
  {-# INLINE lookAhead #-}

instance (LookAheadParsing m, Monad m) => LookAheadParsing (IdentityT m) where
  lookAhead = IdentityT . lookAhead . runIdentityT
  {-# INLINE lookAhead #-}

#ifdef MIN_VERSION_parsec
instance (Parsec.Stream s m t, Show t) => LookAheadParsing (Parsec.ParsecT s u m) where
  lookAhead = Parsec.lookAhead
#endif

#ifdef MIN_VERSION_attoparsec
instance Att.Chunk i => LookAheadParsing (Att.Parser i) where
  lookAhead = Att.lookAhead
#endif

#ifdef MIN_VERSION_binary
instance LookAheadParsing B.Get where
  lookAhead = B.lookAhead
#endif

instance LookAheadParsing ReadP.ReadP where
  lookAhead p = ReadP.look >>= \s ->
                ReadP.choice $ map (return . fst) $ ReadP.readP_to_S p s
