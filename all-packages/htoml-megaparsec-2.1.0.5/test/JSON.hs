{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module JSON where

import           Data.Aeson.Types
import qualified Data.HashMap.Strict as M
import           Data.Text           (Text)
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Text.Toml.Types

-- * Regular ToJSON instances

-- | 'ToJSON' instances for the 'Node' type that produce Aeson (JSON)
-- in line with the TOML specification.
instance ToJSON Node where
  toJSON (VTable v)    = toJSON v
  toJSON (VTArray v)   = toJSON v
  toJSON (VString v)   = toJSON v
  toJSON (VInteger v)  = toJSON v
  toJSON (VFloat v)    = toJSON v
  toJSON (VBoolean v)  = toJSON v
  toJSON (VDatetime v) = toJSON v
  toJSON (VArray v)    = toJSON v

-- * Special BurntSushi ToJSON type class and instances

-- | Type class for conversion to BurntSushi-style JSON.
--
-- BurntSushi has made a language agnostic test suite available that
-- this library uses. This test suite expects that values are encoded
-- as JSON objects with a 'type' and a 'value' member.
class ToBsJSON a where
  toBsJSON :: a -> Value

-- | Provide a 'toBsJSON' instance to the 'VTArray'.
instance (ToBsJSON a) => ToBsJSON (Vector a) where
  toBsJSON = Array . V.map toBsJSON
  {-# INLINE toBsJSON #-}

-- | Provide a 'toBsJSON' instance to the 'NTable'.
instance (ToBsJSON v) => ToBsJSON (M.HashMap Text v) where
  toBsJSON = Object . M.map toBsJSON
  {-# INLINE toBsJSON #-}

-- | 'ToBsJSON' instances for the 'TValue' type that produce Aeson (JSON)
-- in line with BurntSushi's language agnostic TOML test suite.
--
-- As seen in this function, BurntSushi's JSON encoding explicitly
-- specifies the types of the values.
instance ToBsJSON Node where
  toBsJSON (VTable v)    = toBsJSON v
  toBsJSON (VTArray v)   = toBsJSON v
  toBsJSON (VString v)   = object [ "type"  .= toJSON ("string" :: String)
                                  , "value" .= toJSON v ]
  toBsJSON (VInteger v)  = object [ "type"  .= toJSON ("integer" :: String)
                                  , "value" .= toJSON (show v) ]
  toBsJSON (VFloat v)    = object [ "type"  .= toJSON ("float" :: String)
                                  , "value" .= toJSON (show v) ]
  toBsJSON (VBoolean v)  = object [ "type"  .= toJSON ("bool" :: String)
                                  , "value" .= toJSON (if v then "true" else "false" :: String) ]
  toBsJSON (VDatetime v) = object [ "type"  .= toJSON ("datetime" :: String)
                                  , "value" .= toJSON (let s = show v
                                                           z = take (length s - 4)  s ++ "Z"
                                                           d = take (length z - 10) z
                                                           t = drop (length z - 9)  z
                                                       in  d ++ "T" ++ t) ]
  toBsJSON (VArray v)    = object [ "type"  .= toJSON ("array" :: String)
                                  , "value" .= toBsJSON v ]
