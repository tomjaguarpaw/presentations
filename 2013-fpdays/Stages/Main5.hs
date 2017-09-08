{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Main where

import Prelude hiding (map, foldr, (.), (=<<))
import Data.Char (isAlpha, toUpper, isSpace)
import Helpers (getLines, runTests)


-- Module implementation

upperOfString :: String -> String
upperOfString = map toUpper

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x rest
  where rest = foldr f z xs

removeNonAlpha :: String -> String
removeNonAlpha = foldr perhapsCons []

perhapsCons :: Char -> String -> String
perhapsCons x rest = if isAlpha x || isSpace x
                     then x : rest
                     else rest

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : rest
  where rest = map f xs

processNames :: String -> [String] -> [String]
processNames surname = map g
  where g x = (announce . removeNonAlpha . fullName) x
          where announce y = "THERE IS A FAMILY MEMBER CALLED " ++ y
                fullName y = upperOfString y ++ " " ++ upperOfString surname

run :: IO ()
run = do
  putStrLn "Enter first names followed by an empty line"
  names <- getLines
  putStrLn "Enter surname"
  surname <- getLine
  let fullNames = processNames surname names
  putStrLn "The family details are:"
  mapM_ putStrLn fullNames


-- Combinators

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f (g x)

(=<<) :: Monad m => (a -> m b) -> m a -> m b
f =<< m = do
  x <- m
  f x


-- Test and run

main :: IO ()
main = do
  runTests upperOfString removeNonAlpha processNames
  run
