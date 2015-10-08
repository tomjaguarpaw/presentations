{-# LANGUAGE Arrows #-}

module OpalistArrow where

import SchemaArrow
import Opalist (aggregate, groupBy, sumA, runQuery)
import OpalistMonad (guard)
import qualified Data.Profunctor.Product as PP
import Control.Arrow (returnA, Kleisli(Kleisli))

-- 'Kleisli [] a b' is just 'a -> [b]'

type QueryArr a b = Kleisli [] a b
type Query a = QueryArr () a

printRows :: Show a => Query a -> IO ()
printRows = mapM_ print . runQuery

-- # Projection

workplace :: Query (String, String)
workplace = proc () -> do
  employee <- employees -< ()
  returnA -< (eName employee, eCountry employee)


-- # Restriction

ukEmployees :: Query Employee
ukEmployees = proc () -> do
  employee <- employees -< ()

  restrict -< eCountry employee == "UK"

  returnA -< employee

restrict :: QueryArr Bool ()
restrict = Kleisli guard

-- Recall: guard :: Bool -> [()]


-- # Join
--
-- Join is just restriction on an equality

managerOf :: Query (String, String)
managerOf = proc () -> do
  employee               <- employees -< ()
  (department', manager) <- managers  -< ()

  restrict -< eDepartment employee == department'

  returnA -< (eName employee, manager)



-- # Aggregation

linesByEmployeeCountry :: Query (String, String, Int)
linesByEmployeeCountry = proc () -> do
  (employee, _, country, lines) <- output -< ()
  returnA -< (employee, country, lines)

totalLinesByEmployeeCountry :: Query (String, String, Int)
totalLinesByEmployeeCountry =
  aggregate (PP.p3 (groupBy, groupBy, sumA)) linesByEmployeeCountry



-- # Bad aggregation can't happen with arrows

linesByEmployeeIn :: QueryArr String (String, Int)
linesByEmployeeIn = proc country -> do
  (employee, _, country', lines) <- output -< ()
  restrict -< country' == country
  returnA -< (employee, lines)

{- 

  We can't write 'totalLinesByEmployeeIn' because it's structure is
syntactically ruled out by arrow notation, given that aggregation
works on 'QueryArr () r' rather than 'QueryArr a r'.

totalLinesByEmployeeIn :: QueryArr String (String, Int)
totalLinesByEmployeeIn = proc country -> do
  aggregate (PP.p2 (groupBy, sumA)) linesByEmployeeIn -< country

 Couldn't match type `String' with `()'
    Expected type: QueryArr () (String, Int)
      Actual type: QueryArr String (String, Int)
-}



{- # Exercises

  Easy ones to start with:

  1. Project employee and department from the employees table
  2. Restrict the output table to the work which was done on
     Tuesday.
  3. Find all days that each employee was working at their home
     location.

  And now some harder ones:

  3. Calculate the total lines of code output by each employee
     (across all days and locations).
  4. Calculate the total lines of code output on each day
     (across all employees and locations).
  5. Find the maximum number of lines written at each location
     (using the maxA aggregator).
  6. Find the total lines of code output by each employee
     whilst they were working at their home location.
-}


