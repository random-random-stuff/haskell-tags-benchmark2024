module Text.Toml ( parseTomlDoc
                 , parseErrorPretty
                 , Table
                 , Node (..)
                 ) where

import           Control.Monad.State (evalState)
import           Data.Text           (Text)
import           Text.Megaparsec
import           Text.Toml.Parser

-- | Parse a 'Text' that results in 'Either' a 'TomlError'
-- containing the error message, or an internal representation
-- of the document as a 'Table'.
parseTomlDoc :: FilePath -- ^ For error generation
             -> Text -- ^ TOML source
             -> Either TomlError Table
parseTomlDoc _ = flip evalState mempty . runParserT tomlDoc "noneSrc"
