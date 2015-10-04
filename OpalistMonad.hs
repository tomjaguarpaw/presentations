module OpalistMonad where

import Schema
import Opalist (aggregateList, groupBy, sumA, printRows)
import qualified Data.Profunctor.Product as PP

-- # Projection

workplace :: [(String, String)]
workplace = do
  (employee, _, country) <- employees
  return (employee, country)



-- # Restriction

-- First try
ukEmployees1 :: [(String, String, String)]
ukEmployees1 = filter ((== "UK") . third) $ do
  employeeRow <- employees
  return employeeRow

third :: (a, b, c) -> c
third (_, _, z) = z



-- Second try
ukEmployees2 :: [(String, String, String)]
ukEmployees2 = do
  employeeRow <- employees

  -- Mysterious restriction functionality
  if third employeeRow == "UK" then
    [()]
  else
    []

  return employeeRow



-- guard is in Control.Monad
guard :: Bool -> [()]
guard True  = [()]
guard False = []

-- Third try
ukEmployees3 :: [(String, String, String)]
ukEmployees3 = do
  employeeRow <- employees

  guard (third employeeRow == "UK")

  return employeeRow




-- # Join
--
-- Join is just restriction on an equality

managerOf :: [(String, String)]
managerOf = do
  (employee, department, _) <- employees
  (department', manager)    <- managers

  guard (department == department')

  return (employee, manager)




-- # Aggregation

linesByEmployeeCountry :: [(String, String, Int)]
linesByEmployeeCountry = do
  (employee, _, country, lines) <- output
  return (employee, country, lines)

totalLinesByEmployeeCountry :: [(String, String, Int)]
totalLinesByEmployeeCountry =
  aggregateList (PP.p3 (groupBy, groupBy, sumA)) linesByEmployeeCountry


-- # Aggregation that doesn't transfer to SQL

linesByEmployeeIn :: String -> [(String, Int)]
linesByEmployeeIn country = do
  (employee, _, country', lines) <- output
  guard (country' == country)
  return (employee, lines)

totalLinesByEmployeeIn :: String -> [(String, Int)]
totalLinesByEmployeeIn country =
  aggregateList (PP.p2 (groupBy, sumA)) (linesByEmployeeIn country)

linesByEmployeeAtHome :: [(String, Int)]
linesByEmployeeAtHome = do
  (employee, _, country) <- employees
  (employee', lines) <- totalLinesByEmployeeIn country

  guard (employee == employee')

  return (employee, lines)

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
