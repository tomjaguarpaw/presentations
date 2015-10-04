module Schema where

employee :: [(String, String)]
employee = [ ("Tom",   "Back end")
           , ("Neil",  "Back end")
           , ("Alice", "Front end")
           , ("John",  "Front end")
           , ("Anne",  "Testing")
           , ("Ryan",  "Testing") ]

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

manager :: [(String, String)]
manager = [ ("Back end",  "Tom")
          , ("Front end", "Alice")
          , ("Testing",   "Anne") ]
