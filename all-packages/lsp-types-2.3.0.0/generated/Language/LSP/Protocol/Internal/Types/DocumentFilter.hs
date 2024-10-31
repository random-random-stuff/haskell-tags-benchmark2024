{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentFilter where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.NotebookCellTextDocumentFilter
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentFilter
import qualified Language.LSP.Protocol.Types.Common

{-|
A document filter describes a top level text document or
a notebook cell document.

@since 3.17.0 - proposed support for NotebookCellTextDocumentFilter.
-}
newtype DocumentFilter = DocumentFilter (Language.LSP.Protocol.Internal.Types.TextDocumentFilter.TextDocumentFilter Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.NotebookCellTextDocumentFilter.NotebookCellTextDocumentFilter)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DocumentFilter)
