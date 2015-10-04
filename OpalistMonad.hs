module OpalistMonad where

import Schema
import Nopaleye (printRows, aggregateList, groupBy, groupBy, sumA)
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
