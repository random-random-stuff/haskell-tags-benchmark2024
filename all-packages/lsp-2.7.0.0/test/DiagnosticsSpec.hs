{-# LANGUAGE OverloadedStrings #-}

module DiagnosticsSpec where

import Data.HashMap.Strict qualified as HM
import Data.Map qualified as Map
import Data.SortedList qualified as SL
import Data.Text (Text)
import Language.LSP.Diagnostics
import Language.LSP.Protocol.Types qualified as LSP

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Diagnostics functions" diagnosticsSpec

-- -- |Used when running from ghci, and it sets the current directory to ./tests
-- tt :: IO ()
-- tt = do
--   cd ".."
--   hspec spec

-- ---------------------------------------------------------------------

mkDiagnostic :: Maybe Text -> Text -> LSP.Diagnostic
mkDiagnostic ms str =
  let
    rng = LSP.Range (LSP.Position 0 1) (LSP.Position 3 0)
    loc = LSP.Location (LSP.Uri "file") rng
   in
    LSP.Diagnostic rng Nothing Nothing Nothing ms str Nothing (Just [LSP.DiagnosticRelatedInformation loc str]) Nothing

mkDiagnostic2 :: Maybe Text -> Text -> LSP.Diagnostic
mkDiagnostic2 ms str =
  let
    rng = LSP.Range (LSP.Position 4 1) (LSP.Position 5 0)
    loc = LSP.Location (LSP.Uri "file") rng
   in
    LSP.Diagnostic rng Nothing Nothing Nothing ms str Nothing (Just [LSP.DiagnosticRelatedInformation loc str]) Nothing

-- ---------------------------------------------------------------------

diagnosticsSpec :: Spec
diagnosticsSpec = do
  describe "constructs a new store" $ do
    it "constructs a store with no doc version and a single source" $ do
      let
        diags =
          [ mkDiagnostic (Just "hlint") "a"
          , mkDiagnostic (Just "hlint") "b"
          ]
        uri = LSP.toNormalizedUri $ LSP.Uri "uri"
      (updateDiagnostics HM.empty uri Nothing (partitionBySource diags))
        `shouldBe` HM.fromList
          [ (uri, StoreItem Nothing $ Map.fromList [(Just "hlint", SL.toSortedList diags)])
          ]

    -- ---------------------------------

    it "constructs a store with no doc version and multiple sources" $ do
      let
        diags =
          [ mkDiagnostic (Just "hlint") "a"
          , mkDiagnostic (Just "ghcmod") "b"
          ]
        uri = LSP.toNormalizedUri $ LSP.Uri "uri"
      (updateDiagnostics HM.empty uri Nothing (partitionBySource diags))
        `shouldBe` HM.fromList
          [
            ( uri
            , StoreItem Nothing $
                Map.fromList
                  [ (Just "hlint", SL.singleton (mkDiagnostic (Just "hlint") "a"))
                  , (Just "ghcmod", SL.singleton (mkDiagnostic (Just "ghcmod") "b"))
                  ]
            )
          ]

    -- ---------------------------------

    it "constructs a store with doc version and multiple sources" $ do
      let
        diags =
          [ mkDiagnostic (Just "hlint") "a"
          , mkDiagnostic (Just "ghcmod") "b"
          ]
        uri = LSP.toNormalizedUri $ LSP.Uri "uri"
      (updateDiagnostics HM.empty uri (Just 1) (partitionBySource diags))
        `shouldBe` HM.fromList
          [
            ( uri
            , StoreItem (Just 1) $
                Map.fromList
                  [ (Just "hlint", SL.singleton (mkDiagnostic (Just "hlint") "a"))
                  , (Just "ghcmod", SL.singleton (mkDiagnostic (Just "ghcmod") "b"))
                  ]
            )
          ]

  -- ---------------------------------

  describe "updates a store for same document version" $ do
    it "updates a store without a document version, single source only" $ do
      let
        diags1 =
          [ mkDiagnostic (Just "hlint") "a1"
          , mkDiagnostic (Just "hlint") "b1"
          ]
        diags2 =
          [ mkDiagnostic (Just "hlint") "a2"
          ]
        uri = LSP.toNormalizedUri $ LSP.Uri "uri"
      let origStore = updateDiagnostics HM.empty uri Nothing (partitionBySource diags1)
      (updateDiagnostics origStore uri Nothing (partitionBySource diags2))
        `shouldBe` HM.fromList
          [ (uri, StoreItem Nothing $ Map.fromList [(Just "hlint", SL.toSortedList diags2)])
          ]

    -- ---------------------------------

    it "updates just one source of a 2 source store" $ do
      let
        diags1 =
          [ mkDiagnostic (Just "hlint") "a1"
          , mkDiagnostic (Just "ghcmod") "b1"
          ]
        diags2 =
          [ mkDiagnostic (Just "hlint") "a2"
          ]
        uri = LSP.toNormalizedUri $ LSP.Uri "uri"
      let origStore = updateDiagnostics HM.empty uri Nothing (partitionBySource diags1)
      (updateDiagnostics origStore uri Nothing (partitionBySource diags2))
        `shouldBe` HM.fromList
          [
            ( uri
            , StoreItem Nothing $
                Map.fromList
                  [ (Just "hlint", SL.singleton (mkDiagnostic (Just "hlint") "a2"))
                  , (Just "ghcmod", SL.singleton (mkDiagnostic (Just "ghcmod") "b1"))
                  ]
            )
          ]

    -- ---------------------------------

    it "updates just one source of a 2 source store, with empty diags" $ do
      let
        diags1 =
          [ mkDiagnostic (Just "hlint") "a1"
          , mkDiagnostic (Just "ghcmod") "b1"
          ]
        uri = LSP.toNormalizedUri $ LSP.Uri "uri"
      let origStore = updateDiagnostics HM.empty uri Nothing (partitionBySource diags1)
      (updateDiagnostics origStore uri Nothing (Map.fromList [(Just "ghcmod", SL.toSortedList [])]))
        `shouldBe` HM.fromList
          [
            ( uri
            , StoreItem Nothing $
                Map.fromList
                  [ (Just "ghcmod", SL.toSortedList [])
                  , (Just "hlint", SL.singleton (mkDiagnostic (Just "hlint") "a1"))
                  ]
            )
          ]

  -- ---------------------------------

  describe "updates a store for a new document version" $ do
    it "updates a store without a document version, single source only" $ do
      let
        diags1 =
          [ mkDiagnostic (Just "hlint") "a1"
          , mkDiagnostic (Just "hlint") "b1"
          ]
        diags2 =
          [ mkDiagnostic (Just "hlint") "a2"
          ]
        uri = LSP.toNormalizedUri $ LSP.Uri "uri"
      let origStore = updateDiagnostics HM.empty uri (Just 1) (partitionBySource diags1)
      (updateDiagnostics origStore uri (Just 2) (partitionBySource diags2))
        `shouldBe` HM.fromList
          [ (uri, StoreItem (Just 2) $ Map.fromList [(Just "hlint", SL.toSortedList diags2)])
          ]

    -- ---------------------------------

    it "updates a store for a new doc version, removing all priot sources" $ do
      let
        diags1 =
          [ mkDiagnostic (Just "hlint") "a1"
          , mkDiagnostic (Just "ghcmod") "b1"
          ]
        diags2 =
          [ mkDiagnostic (Just "hlint") "a2"
          ]
        uri = LSP.toNormalizedUri $ LSP.Uri "uri"
      let origStore = updateDiagnostics HM.empty uri (Just 1) (partitionBySource diags1)
      (updateDiagnostics origStore uri (Just 2) (partitionBySource diags2))
        `shouldBe` HM.fromList
          [
            ( uri
            , StoreItem (Just 2) $
                Map.fromList
                  [ (Just "hlint", SL.singleton (mkDiagnostic (Just "hlint") "a2"))
                  ]
            )
          ]

  -- ---------------------------------

  describe "retrieves all the diagnostics for a given uri" $ do
    it "gets diagnostics for multiple sources" $ do
      let
        diags =
          [ mkDiagnostic (Just "hlint") "a"
          , mkDiagnostic (Just "ghcmod") "b"
          ]
        uri = LSP.toNormalizedUri $ LSP.Uri "uri"
      let ds = updateDiagnostics HM.empty uri (Just 1) (partitionBySource diags)
      getDiagnosticParamsFor 10 ds uri
        `shouldBe` Just (LSP.PublishDiagnosticsParams (LSP.fromNormalizedUri uri) (Just 1) (reverse diags))

  -- ---------------------------------

  describe "limits the number of diagnostics retrieved, in order" $ do
    it "gets diagnostics for multiple sources" $ do
      let
        diags =
          [ mkDiagnostic2 (Just "hlint") "a"
          , mkDiagnostic2 (Just "ghcmod") "b"
          , mkDiagnostic (Just "hlint") "c"
          , mkDiagnostic (Just "ghcmod") "d"
          ]
        uri = LSP.toNormalizedUri $ LSP.Uri "uri"
      let ds = updateDiagnostics HM.empty uri (Just 1) (partitionBySource diags)
      getDiagnosticParamsFor 2 ds uri
        `shouldBe` Just
          ( LSP.PublishDiagnosticsParams
              (LSP.fromNormalizedUri uri)
              (Just 1)
              [ mkDiagnostic (Just "ghcmod") "d"
              , mkDiagnostic (Just "hlint") "c"
              ]
          )

      getDiagnosticParamsFor 1 ds uri
        `shouldBe` Just
          ( LSP.PublishDiagnosticsParams
              (LSP.fromNormalizedUri uri)
              (Just 1)
              [ mkDiagnostic (Just "ghcmod") "d"
              ]
          )

  -- ---------------------------------

  describe "flushes the diagnostics for a given source" $ do
    it "gets diagnostics for multiple sources" $ do
      let
        diags =
          [ mkDiagnostic2 (Just "hlint") "a"
          , mkDiagnostic2 (Just "ghcmod") "b"
          , mkDiagnostic (Just "hlint") "c"
          , mkDiagnostic (Just "ghcmod") "d"
          ]
        uri = LSP.toNormalizedUri $ LSP.Uri "uri"
      let ds = updateDiagnostics HM.empty uri (Just 1) (partitionBySource diags)
      getDiagnosticParamsFor 100 ds uri
        `shouldBe` Just
          ( LSP.PublishDiagnosticsParams
              (LSP.fromNormalizedUri uri)
              (Just 1)
              [ mkDiagnostic (Just "ghcmod") "d"
              , mkDiagnostic (Just "hlint") "c"
              , mkDiagnostic2 (Just "ghcmod") "b"
              , mkDiagnostic2 (Just "hlint") "a"
              ]
          )

      let ds' = flushBySource ds (Just "hlint")
      getDiagnosticParamsFor 100 ds' uri
        `shouldBe` Just
          ( LSP.PublishDiagnosticsParams
              (LSP.fromNormalizedUri uri)
              (Just 1)
              [ mkDiagnostic (Just "ghcmod") "d"
              , mkDiagnostic2 (Just "ghcmod") "b"
              ]
          )

-- ---------------------------------
