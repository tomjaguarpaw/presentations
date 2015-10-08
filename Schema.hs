module Schema where

data Employee = Employee {
    eName       :: String
  , eDepartment :: String
  , eCountry    :: String
  }

employees :: [Employee]
employees = [ Employee "Tom"   "Back end"  "UK"
            , Employee "Neil"  "Back end"  "UK"
            , Employee "Alice" "Front end" "FR"
            , Employee "John"  "Front end" "UK"
            , Employee "Anne"  "Testing"   "FR"
            , Employee "Ryan"  "Testing"   "FR" ]

output :: [(String, String, String, Int)]
output = [ ("Tom",   "Monday",  "UK", 530)
         , ("Tom",   "Tuesday", "UK", 320)
         , ("Neil",  "Monday",  "UK", 230)
         , ("Neil",  "Tuesday", "FR", 950)
         , ("Alice", "Monday",  "FR", 630)
         , ("Alice", "Tuesday", "FR", 350)
         , ("John",  "Monday",  "FR", 220)
         , ("John",  "Tuesday", "UK", 320)
         , ("Anne",  "Monday",  "UK", 420)
         , ("Anne",  "Tuesday", "FR", 690)
         , ("Ryan",  "Monday",  "UK", 360)
         , ("Ryan",  "Tuesday", "UK", 380) ]

managers :: [(String, String)]
managers = [ ("Back end",  "Tom")
           , ("Front end", "Alice")
           , ("Testing",   "Anne") ]
