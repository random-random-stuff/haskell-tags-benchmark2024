{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad

import qualified Data.ByteString as B

import System.INotify as INotify

import Utils

write :: FilePath -> IO ()
write path =
    B.writeFile (path ++ "/hello") ""
    -- actually writing any contents gives me two Modified
    
main :: IO ()
main =
    inTestEnviron [AllEvents] write $ \ events -> do
        when (expected ~= events)
            testSuccess
        putStrLn $ explainFailure expected events
        testFailure

expected :: [Event]
expected =
    [ Created   False "hello"
    , Opened    False (Just "hello")
    , Modified  False (Just "hello")
    , Closed    False (Just "hello") True
    ]
