module OpalistMonad where

import Schema
import Opalist (aggregateList, groupBy, sumA, printRows)
import qualified Data.Profunctor.Product as PP

-- # Projection

workplace :: [(String, String)]
workplace = do
  employee <- employees
  return (eName employee, eCountry employee)



-- # Restriction

-- First try
ukEmployees1 :: [Employee]
ukEmployees1 = filter ((== "UK") . eCountry) $ do
  employee <- employees
  return employee

third :: (a, b, c) -> c
third (_, _, z) = z



-- Second try
ukEmployees2 :: [Employee]
ukEmployees2 = do
  employee <- employees

  -- Mysterious restriction functionality
  if eCountry employee == "UK" then
    [()]
  else
    []

  return employee



-- guard is in Control.Monad
guard :: Bool -> [()]
guard True  = [()]
guard False = []

-- Third try
ukEmployees3 :: [Employee]
ukEmployees3 = do
  employee <- employees

  guard (eCountry employee == "UK")

  return employee




-- # Join
--
-- Join is just restriction on an equality

managerOf :: [(String, String)]
managerOf = do
  employee <- employees
  manager  <- managers

  guard (eDepartment employee == mDepartment manager)

  return (eName employee, mManager manager)




-- # Aggregation

linesByEmployeeCountry :: [(String, String, Int)]
linesByEmployeeCountry = do
  outputRow <- output
  return (oName outputRow, oCountry outputRow, oOutput outputRow)

totalLinesByEmployeeCountry :: [(String, String, Int)]
totalLinesByEmployeeCountry =
  aggregateList (PP.p3 (groupBy, groupBy, sumA)) linesByEmployeeCountry


-- # Aggregation that doesn't transfer to SQL

linesByEmployeeIn :: String -> [(String, Int)]
linesByEmployeeIn country = do
  outputRow <- output
  guard (oCountry outputRow == country)
  return (oName outputRow, oOutput outputRow)

totalLinesByEmployeeIn :: String -> [(String, Int)]
totalLinesByEmployeeIn country =
  aggregateList (PP.p2 (groupBy, sumA)) (linesByEmployeeIn country)

linesByEmployeeAtHome :: [(String, Int)]
linesByEmployeeAtHome = do
  employee <- employees
  (employee', lines) <- totalLinesByEmployeeIn (eCountry employee)

  guard (eName employee == employee')

  return (eName employee, lines)

{-

  linesByEmployeeAtHome would generate the following SQL.

  SELECT employee, country FROM employees
         output_employee, lines FROM (
           SELECT output_employee, SUM(lines) FROM output
           WHERE country == output_county
           GROUP BY output_employee) as T1
  WHERE employee == output_employee

  This SQL is invalid because 'country' is not in scope in the line
  'WHERE country == output_county'

-}
