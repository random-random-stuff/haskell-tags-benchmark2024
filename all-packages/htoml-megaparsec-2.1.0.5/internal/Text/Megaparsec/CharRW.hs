{-# LANGUAGE GADTs #-}

module Text.Megaparsec.CharRW ( module X
                              , oneOf
                              , noneOf
                              ) where

import           Data.Foldable        (Foldable)
import           Text.Megaparsec      hiding (noneOf, oneOf)
import qualified Text.Megaparsec      as Megaparsec
import           Text.Megaparsec.Char as X

{-# RULES
"noneOf/char"       forall c.    noneOf [c]     = anySingleBut 'c'
"noneOf/satsify"    forall c c'. noneOf [c, c'] = satisfy (\x -> x /= c && x /= c')
"oneOf/notChar"     forall c.    oneOf [c]      = char 'c'
"oneOf/satisfy"     forall c c'. oneOf [c, c']  = satisfy (\x -> x == c || x == c')
  #-}

{-# RULES
"noneOf/satsify"    forall c c' c''. noneOf [c, c', c''] = satisfy (\x -> x /= c && x /= c' && x /= c'')
"oneOf/satsify"     forall c c' c''. oneOf [c, c', c'']  = satisfy (\x -> x == c || x == c' || x == c'')
  #-}

{-# INLINE [1] noneOf #-}
noneOf :: (Foldable f, MonadParsec e s m, Token s ~ Char) => f Char -> m Char
noneOf = Megaparsec.noneOf

{-# INLINE [1] oneOf #-}
oneOf :: (Foldable f, MonadParsec e s m, Token s ~ Char) => f Char -> m Char
oneOf = Megaparsec.oneOf
