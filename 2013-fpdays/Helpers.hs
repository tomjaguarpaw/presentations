{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Helpers where

import Prelude hiding (lines)
import Control.Monad (when)

-- Helper functions

getLines :: IO [String]
getLines = do
  line <- getLine
  if line == ""
    then return []
    else do
    lines <- getLines
    return (line:lines)

runTests capitalize removeNonAlpha processNames = do
  check (capitalize "hello") "HELLO"
  check (removeNonAlpha "abc 123!@# def") "abc  def"
  check (processNames "Smith" [ "Bob123by", "Sal%$#ly" ])
                              [ "THERE IS A FAMILY MEMBER CALLED BOBBY SMITH"
                              , "THERE IS A FAMILY MEMBER CALLED SALLY SMITH"]

check :: (Eq a, Show a) => a -> a -> IO ()
check a b = when (a /= b) $ do
  putStrLn "Test failed"
  print a
  print b
