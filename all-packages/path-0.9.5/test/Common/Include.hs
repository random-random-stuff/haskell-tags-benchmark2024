-- This template expects CPP definitions for:
--     PLATFORM_NAME = Posix | Windows
--     IS_WINDOWS    = False | True

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Test functions that are common to Posix and Windows

module Common.PLATFORM_NAME (extensionOperations) where

import Control.Monad
import qualified Language.Haskell.TH.Syntax as TH
import Path.Internal.PLATFORM_NAME
import Path.PLATFORM_NAME
import System.FilePath.PLATFORM_NAME (pathSeparator)
import Test.Hspec

class Foo a b where
    foo :: Path a b -> FilePath
    foo = toFilePath

instance Foo Abs Dir
instance Foo Abs File
instance Foo Rel Dir
instance Foo Rel File

qqRelDir :: FilePath
qqRelDir = foo [reldir|foo/|]

qqRelFile :: FilePath
qqRelFile = foo [relfile|foo|]

thRelDir :: FilePath
thRelDir = foo $(mkRelDir "foo/")

thRelFile :: FilePath
thRelFile = foo $(mkRelFile "foo")

liftRelDir :: FilePath
liftRelDir = foo $(TH.lift (Path "foo/" :: Path Rel Dir))

liftRelFile :: FilePath
liftRelFile = foo $(TH.lift (Path "foo" :: Path Rel File))

validExtensionsSpec :: String -> Path b File -> Path b File -> Spec
validExtensionsSpec ext file fext = do
    let f = show $ toFilePath file
    let fx = show $ toFilePath fext

    it ("addExtension " ++ show ext ++ " " ++ f ++ " == " ++ fx) $
        addExtension ext file `shouldReturn` fext

    it ("fileExtension " ++ fx ++ " == " ++ ext) $
        fileExtension fext `shouldReturn` ext

    it ("replaceExtension " ++ show ext ++ " " ++ fx ++ " == " ++ fx) $
        replaceExtension ext fext `shouldReturn` fext

extensionOperations :: String -> Spec
extensionOperations rootDrive = do
    let extension = ".foo"
    let extensions = extension : [".foo.", ".foo.."]

    describe "Only filenames and extensions" $
      forM_ extensions $ \ext ->
          forM_ filenames $ \f -> do
              runTests parseRelFile f ext

    describe "Relative dir paths" $
      forM_ dirnames $ \d -> do
          forM_ filenames $ \f -> do
              let f1 = d ++ [pathSeparator] ++ f
              runTests parseRelFile f1 extension

    describe "Absolute dir paths" $
      forM_ dirnames $ \d -> do
          forM_ filenames $ \f -> do
              let f1 = rootDrive ++ d ++ [pathSeparator] ++ f
              runTests parseAbsFile f1 extension

    -- Invalid extensions
    forM_ invalidExtensions $ \ext -> do
        it ("throws InvalidExtension when extension is [" ++ ext ++ "]")  $
            addExtension ext $(mkRelFile "name")
            `shouldThrow` (== InvalidExtension ext)

    where

    runTests parse file ext = do
        let maybePathFile = parse file
        let maybePathFileWithExt = parse (file ++ ext)
        case (maybePathFile, maybePathFileWithExt) of
            (Just pathFile, Just pathFileWithExt) -> validExtensionsSpec ext pathFile pathFileWithExt
            _ -> it ("Files " ++ show file ++ " and/or " ++ show (file ++ ext) ++ " should parse successfully.") $
                     expectationFailure $
                         show file ++ " parsed to " ++ show maybePathFile ++ ", "
                         ++ show (file ++ ext) ++ " parsed to " ++ show maybePathFileWithExt

    filenames =
        [ "name"
        , "name."
        , "name.."
        , ".name"
        , "..name"
        , "name.name"
        , "name..name"
        , "..."
        ]
    dirnames = filenames ++ ["."]
    invalidExtensions =
        [ ""
        , "."
        , "x"
        , ".."
        , "..."
        , "xy"
        , "foo"
        , "foo."
        , "foo.."
        , "..foo"
        , "...foo"
        , ".foo.bar"
        , ".foo" ++ [pathSeparator] ++ "bar"
        ]
