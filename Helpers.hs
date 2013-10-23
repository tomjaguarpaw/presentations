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

runTests capitalize removeNonAlpha removeNonAlphaList processNames = do
  check (capitalize "hello") "Hello"
  check (removeNonAlpha "abc 123!@# def") "abc  def"
  check (removeNonAlphaList [ "abc 123!@# def" , ")(*&xyz<>." ])
                            [ "abc  def", "xyz" ]
  check (processNames "Smith" [ "Bob123by", "Sal%$#ly" ])
                              [ "There is a family member called Bobby Smith"
                              , "There is a family member called Sally Smith" ]

check :: (Eq a, Show a) => a -> a -> IO ()
check a b = when (a /= b) $ do
  putStrLn "Test failed"
  print a
  print b
