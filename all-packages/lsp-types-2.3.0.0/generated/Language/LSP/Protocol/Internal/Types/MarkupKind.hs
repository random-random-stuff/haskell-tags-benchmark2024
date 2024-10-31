{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.MarkupKind where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Set
import qualified Data.String
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.LspEnum

{-|
Describes the content type that a client supports in various
result literals like `Hover`, `ParameterInfo` or `CompletionItem`.

Please note that `MarkupKinds` must not start with a `$`. This kinds
are reserved for internal usage.
-}
data MarkupKind = 
    {-|
  Plain text is supported as a content format
  -}
  MarkupKind_PlainText
  | {-|
  Markdown is supported as a content format
  -}
  MarkupKind_Markdown
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum MarkupKind)
  deriving Pretty via (ViaJSON MarkupKind)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum MarkupKind where
  knownValues = Data.Set.fromList [MarkupKind_PlainText,MarkupKind_Markdown]
  type EnumBaseType MarkupKind = Data.Text.Text
  toEnumBaseType MarkupKind_PlainText = "plaintext"
  toEnumBaseType MarkupKind_Markdown = "markdown"
  fromEnumBaseType "plaintext" = pure MarkupKind_PlainText
  fromEnumBaseType "markdown" = pure MarkupKind_Markdown
  fromEnumBaseType _ = Nothing


