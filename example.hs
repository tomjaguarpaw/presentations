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

getLines :: IO [String]
getLines = do
  line <- getLine
  if line == ""
    then return []
    else do
    lines <- getLines
    return (line:lines)

main = do
  putStrLn "Enter first names followed by an empty line"
  names <- getLines
  putStrLn "Enter surname"
  surname <- getLine
  let fullNames = processNames surname names
  putStrLn "The familiy details are:"
  mapM_ putStrLn fullNames
