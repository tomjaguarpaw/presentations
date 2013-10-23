{-# OPTIONS_GHC -fno-warn-unused-matches -fno-warn-unused-imports #-}

import Prelude hiding (map, foldr, (.), lines)
import Data.Char (isAlpha, toUpper, isSpace)
import Control.Monad ((<=<))

-- Module implementation

removeNonAlphaList :: [String] -> [String]
removeNonAlphaList [] = []
removeNonAlphaList (x:xs) = removeNonAlpha x : removeNonAlphaList xs

removeNonAlpha :: String -> String
removeNonAlpha [] = []
removeNonAlpha (x:xs) = if isAlpha x || isSpace x
                        then x : removeNonAlpha xs
                        else removeNonAlpha xs

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

processNames :: String -> [String] -> [String]
processNames surname [] = []
processNames surname (name:names) = ret : processNames surname names
  where ret = "There is a family member called " ++ cleanedFullName
        cleanedFullName = removeNonAlpha fullName
        fullName = capitalize name ++ " " ++ capitalize surname

getLines :: IO [String]
getLines = do
  line <- getLine
  if line == ""
    then return []
    else do
    lines <- getLines
    return (line:lines)

run :: IO ()
run = do
  putStrLn "Enter first names followed by an empty line"
  names <- getLines
  putStrLn "Enter surname"
  surname <- getLine
  let fullNames = processNames surname names
  putStrLn "The familiy details are:"
  mapM_ putStrLn fullNames

main :: IO ()
main = run

-- Combinators

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f (g x)

(=<<) :: Monad m => (a -> m b) -> m a -> m b
f =<< m = do
  x <- m
  f x
