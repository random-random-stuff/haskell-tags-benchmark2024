module Main where
import           Test.Tasty            (defaultMain, testGroup)

import qualified BurntSushi
import           Text.Toml.Parser.Spec


main :: IO ()
main = do
  parserSpec <- tomlParserSpec
  bsTests <- BurntSushi.tests

  defaultMain $ testGroup ""
    [ parserSpec
    , bsTests
    ]
