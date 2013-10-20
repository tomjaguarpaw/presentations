import Data.Char (isAlpha, toUpper, isSpace)

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

getFirstNames' :: [String] -> IO [String]
getFirstNames' names = do
  line <- getLine
  if line == ""
    then return names
    else getFirstNames' (names ++ [line])

getFirstNames :: IO [String]
getFirstNames = getFirstNames' []

main = do
  putStrLn "Enter first names followed by an empty line"
  names <- getFirstNames
  putStrLn "Enter surname"
  surname <- getLine
  let fullNames = processNames surname names
  putStrLn "The familiy details are:"
  mapM_ putStrLn fullNames
