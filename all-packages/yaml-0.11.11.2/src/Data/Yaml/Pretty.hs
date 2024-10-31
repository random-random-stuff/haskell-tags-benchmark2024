{-# LANGUAGE CPP #-}
-- | Prettier YAML encoding.
--
-- @since 0.8.13
module Data.Yaml.Pretty
    ( encodePretty
    , Config
    , getConfCompare
    , setConfCompare
    , getConfDropNull
    , setConfDropNull
    , defConfig
    , pretty
    ) where

import Prelude hiding (null)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Data.Bifunctor (first)
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as HM
#else
import qualified Data.HashMap.Strict as HM
#endif
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.Function (on)
import Data.List (sortBy)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif
import Data.Text (Text)
import qualified Data.Vector as V

import Data.Yaml.Builder

#if MIN_VERSION_aeson(2,0,0)
toText :: Key -> Text
toText = K.toText
#else
toText :: Key -> Text
toText = id

type Key = Text
#endif

-- |
-- @since 0.8.13
data Config = Config
  { confCompare :: Text -> Text -> Ordering -- ^ Function used to sort keys in objects
  , confDropNull :: Bool -- ^ Drop null values from objects
  }

-- | The default configuration: do not sort objects or drop keys
--
-- @since 0.8.13
defConfig :: Config
defConfig = Config mempty False

-- |
-- @since 0.8.13
getConfCompare :: Config -> Text -> Text -> Ordering
getConfCompare = confCompare

-- | Sets ordering for object keys
--
-- @since 0.8.13
setConfCompare :: (Text -> Text -> Ordering) -> Config -> Config
setConfCompare cmp c = c { confCompare = cmp }

-- |
-- @since 0.8.24
getConfDropNull :: Config -> Bool
getConfDropNull = confDropNull

-- | Drop entries with `Null` value from objects, if set to `True`
--
-- @since 0.8.24
setConfDropNull :: Bool -> Config -> Config
setConfDropNull m c = c { confDropNull = m }

pretty :: Config -> Value -> YamlBuilder
pretty cfg = go
  where go (Object o) = let sort = sortBy (confCompare cfg `on` fst)
                            select
                              | confDropNull cfg = HM.filter (/= Null)
                              | otherwise        = id
                        in mapping (sort $ fmap (first toText) $ HM.toList $ HM.map go $ select o)
        go (Array a)  = array (go <$> V.toList a)
        go Null       = null
        go (String s) = string s
        go (Number n) = scientific n
        go (Bool b)   = bool b

-- | Configurable 'encode'.
--
-- @since 0.8.13
encodePretty :: ToJSON a => Config -> a -> ByteString
encodePretty cfg = toByteString . pretty cfg . toJSON
