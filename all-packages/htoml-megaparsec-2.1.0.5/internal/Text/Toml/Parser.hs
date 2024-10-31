{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Text.Toml.Parser
  ( module Text.Toml.Parser
  , module Text.Toml.Types
  ) where

import           Control.Monad
import           Control.Monad.State      (evalState)
import           Data.Functor
import qualified Data.HashMap.Lazy        as M
import qualified Data.Set                 as S
import           Data.Text                (Text, pack, unpack)
import qualified Data.Text                as T
import           Data.Time.Format.ISO8601 (formatParseM, iso8601Format)
import qualified Data.Vector              as V
import           Data.Void
import           Numeric                  (readHex)
import           Text.Megaparsec          hiding (noneOf, oneOf)
import           Text.Megaparsec.CharRW
import           Text.Toml.Types

type TomlError = ParseErrorBundle Text Void

-- | Convenience function for the test suite and GHCI.
parseOnly :: Parser Toml a -> Text -> Either TomlError a
parseOnly parser = flip evalState mempty . runParserT parser "noneSrc"

-- | Parses a complete document formatted according to the TOML spec.
tomlDoc :: (TomlM m) => Parser m Table
tomlDoc = do
    skipBlanks
    topTable <- table
    namedSections <- many namedSection
    -- Ensure the input is completely consumed
    eof
    -- Load each named section into the top table
    foldM (\x y -> insert Explicit y x) topTable namedSections

-- | Parses a table of key-value pairs.
table :: (TomlM m) => Parser m Table
table = do
    pairs <- many (assignment <* skipBlanks) <|> (skipBlanks >> pure [])
    case maybeDupe (map fst pairs) of
      Just k  -> throwParser $ "Cannot redefine key '" ++ unpack k ++ "'"
      Nothing -> return $ M.fromList pairs

-- | Parses an inline table of key-value pairs.
inlineTable :: (TomlM m) => Parser m (Either (S.Set (ErrorFancy Void)) Node)
inlineTable = do
    pairs <- between (char '{') (char '}') (skipSpaces *> separatedValues <* skipSpaces)
    case maybeDupe (map fst pairs) of
      Just k  ->
        pure $ Left (S.fromList [ErrorFail $ "Cannot redefine key " ++ unpack k ])
      Nothing -> pure $ Right $ VTable $ M.fromList pairs
  where
    skipSpaces      = many (satisfy isSpc)
    separatedValues = sepBy (skipSpaces *> assignment <* skipSpaces) comma
    comma           = skipSpaces >> char ',' >> skipSpaces

-- | Find dupes, if any.
maybeDupe :: Ord a => [a] -> Maybe a
maybeDupe xx = dup xx S.empty
  where
    dup []     _ = Nothing
    dup (x:xs) s = if S.member x s then Just x else dup xs (S.insert x s)


-- | Parses a 'Table' or 'TableArray' with its header.
-- The resulting tuple has the header's value in the first position, and the
-- 'NTable' or 'NTArray' in the second.
namedSection :: (TomlM m) => Parser m ([Text], Node)
namedSection = do
    eitherHdr <- try (Left <$> tableHeader) <|> (Right <$> tableArrayHeader)
    skipBlanks
    tbl <- table
    skipBlanks
    return $ case eitherHdr of Left  ns -> (ns, VTable tbl )
                               Right ns -> (ns, VTArray $ V.singleton tbl)


-- | Parses a table header.
tableHeader :: (TomlM m) => Parser m [Text]
tableHeader = between (char '[') (char ']') headerValue


-- | Parses a table array header.
tableArrayHeader :: (TomlM m) => Parser m [Text]
tableArrayHeader = between (string "[[") (string "]]") headerValue


-- | Parses the value of any header (names separated by dots), into a list of 'Text'.
headerValue :: (TomlM m) => Parser m [Text]
headerValue = ((pack <$> some keyChar) <|> anyStr') `sepBy1` char '.'
  where
    keyChar = alphaNumChar <|> oneOf ("-_" :: String)

-- | Parses a key-value assignment.
assignment :: (TomlM m) => Parser m (Text, Node)
assignment = do
    k <- (pack <$> some keyChar) <|> anyStr'
    many (satisfy isSpc) >> char '=' >> skipBlanks
    v' <- value
    v <- case v' of
        Right x -> pure x
        Left y  -> fancyFailure y
    return (k, v)
  where
    -- TODO: Follow the spec, e.g.: only first char cannot be '['.
    keyChar = alphaNumChar <|> oneOf ("-_" :: String)


-- | Parses a value.
value :: (TomlM m) => Parser m (Either (S.Set (ErrorFancy Void)) Node)
value = (pure <$> try array       <?> "array")
    <|> (pure <$> try boolean     <?> "boolean")
    <|> (pure <$> try anyStr      <?> "string")
    <|> (pure <$> try datetime    <?> "datetime")
    <|> (pure <$> try float       <?> "float")
    <|> (pure <$> try integer     <?> "integer")
    <|> (inlineTable     <?> "inline table")


--
-- | * Toml value parsers
--

array :: (TomlM m) => Parser m Node
array = (try (arrayOf array)    <?> "array of arrays")
    <|> (try (arrayOf boolean)  <?> "array of booleans")
    <|> (try (arrayOf anyStr)   <?> "array of strings")
    <|> (try (arrayOf datetime) <?> "array of datetimes")
    <|> (try (arrayOf float)    <?> "array of floats")
    <|> (arrayOf integer        <?> "array of integers")


boolean :: (TomlM m) => Parser m Node
boolean = VBoolean <$> ( string "true"  $> True  <|>
                         string "false" $> False )

anyStr :: (TomlM m) => Parser m Node
anyStr = VString <$> anyStr'

anyStr' :: (TomlM m) => Parser m Text
anyStr' = try multiBasicStr <|> try basicStr <|> try multiLiteralStr <|> literalStr

basicStr :: (TomlM m) => Parser m Text
basicStr = between dQuote dQuote (pack <$> many strChar)
  where
    strChar = escSeq <|> noneOf ("\"\\" :: String)
    dQuote  = char '\"'

multiBasicStr :: (TomlM m) => Parser m Text
multiBasicStr = openDQuote3 *> escWhiteSpc *> (pack <$> manyTill strChar (try dQuote3))
  where
    -- | Parse the a tripple-double quote, with possibly a newline attached
    openDQuote3 = try (dQuote3 <* char '\n') <|> dQuote3
    -- | Parse tripple-double quotes
    dQuote3     = count 3 $ char '"'
    -- | Parse a string char, accepting escaped codes, ignoring escaped white space
    strChar     = escSeq <|> noneOf ("\\" :: String) <* escWhiteSpc
    -- | Parse escaped white space, if any
    escWhiteSpc = many $ char '\\' >> char '\n' >> many (oneOf ("\n\t " :: String))


literalStr :: (TomlM m) => Parser m Text
literalStr = between sQuote sQuote (pack <$> many (noneOf ("'" :: String)))
  where
    sQuote = char '\''

multiLiteralStr :: (TomlM m) => Parser m Text
multiLiteralStr = openSQuote3 *> (pack <$> manyTill anySingle sQuote3)
  where
    -- | Parse the a tripple-single quote, with possibly a newline attached
    openSQuote3 = try (sQuote3 <* char '\n') <|> sQuote3
    -- | Parse tripple-single quotes
    sQuote3     = try . count 3 . char $ '\''

datetime :: (TomlM m) => Parser m Node
datetime = do
    d <- manyTill anySingle (char 'Z')
    VDatetime <$> formatParseM iso8601Format (d++"Z")

-- | Attoparsec 'double' parses scientific "e" notation; reimplement according to Toml spec.
float :: (TomlM m) => Parser m Node
float = VFloat <$> do
    n <- intStr <* lookAhead (oneOf (".eE" :: String))
    d <- (char '.' *> uintStr) <|> return "0"
    e <- (oneOf ("eE" :: String) *> intStr) <|> return "0"
    return . read . join $ [n, ".", d, "e", e]
  where
    sign    = (T.singleton <$> char '-') <|> (char '+' >> return "") <|> return ""
    uintStr = (:) <$> digitChar <*> many (optional (char '_') *> digitChar)
    intStr  = do s <- T.unpack <$> sign
                 u <- uintStr
                 return . join $ s : [u]

integer :: (TomlM m) => Parser m Node
integer = VInteger <$> signed (read <$> uintStr)
  where
    uintStr :: (TomlM m) => Parser m String
    uintStr = (:) <$> digitChar <*> many (optional (char '_') *> digitChar)

--
-- * Utility functions
--

-- | Parses the elements of an array, while restricting them to a certain type.
arrayOf :: (TomlM m) => Parser m Node -> Parser m Node
arrayOf p = VArray . V.fromList <$>
                between (char '[') (char ']') (skipBlanks *> separatedValues)
  where
    separatedValues = sepEndBy (skipBlanks *> try p <* skipBlanks) comma <* skipBlanks
    comma           = skipBlanks >> char ',' >> skipBlanks

-- | Parser for escape sequences.
escSeq :: (TomlM m) => Parser m Char
escSeq = char '\\' *> escSeqChar
  where
    escSeqChar =  char '"'
              <|> char '\\'
              <|> char '/'
              <|> char 'n' $> '\n'
              <|> char 't' $> '\t'
              <|> char 'r' $> '\r'
              <|> char 'b' $> '\b'
              <|> char 'f' $> '\f'
              <|> char 'u' *> unicodeHex 4
              <|> char 'U' *> unicodeHex 8
              <?> "escaped character"

-- | Parser for unicode hexadecimal values of representation length 'n'.
unicodeHex :: (TomlM m) => Int -> Parser m Char
unicodeHex n = do
    h <- count n hexDigitChar -- (satisfy isHex)
    let v = fst . head . readHex $ h
    return $ if v <= maxChar then toEnum v else '�'
  where
    -- isHex x = or . sequence [isDigit, isAsciiUpper, isAsciiLower]
    maxChar = fromEnum (maxBound :: Char)

-- | Parser for signs (a plus or a minus).
signed :: (Num a, TomlM m) => Parser m a -> Parser m a
signed p = p
        <|> (negate <$> (char '-' *> p))
        <|> (char '+' *> p)

-- | Parses the (rest of the) line including an EOF, whitespace and comments.
skipBlanks :: (TomlM m) => Parser m ()
skipBlanks = skipMany blank
  where
    blank   = void (some (satisfy isSpc)) <|> comment <|> void eol
    comment = char '#' *> void (many $ noneOf ("\n" :: String))

-- | Results in 'True' for whitespace chars, tab or space, according to spec.
isSpc :: Char -> Bool
isSpc ' '  = True
isSpc '\t' = True
isSpc _    = False
