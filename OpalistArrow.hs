{-# LANGUAGE Arrows #-}

module OpalistArrow where

import SchemaArrow
import Nopaleye (aggregate, groupBy, groupBy, sumA, runQuery)
import OpalistMonad (guard)
import qualified Data.Profunctor.Product as PP
import Control.Arrow

type Query a = QueryArr () a
type QueryArr a b = Kleisli [] a b

-- 'Kleisli [] a b' is just 'a -> [b]'
--
-- 'Query a' is just+ '[a]'
--
-- + isomorphic to

printRows :: Show a => Query a -> IO ()
printRows = mapM_ print . runQuery

-- # Projection

workplace :: Query (String, String)
workplace = proc () -> do
  (employee, _, country) <- employees -< ()
  returnA -< (employee, country)


-- # Restriction

ukEmployees :: Query (String, String, String)
ukEmployees = proc () -> do
  employeeRow <- employees -< ()

  restrict -< third employeeRow == "UK"

  returnA -< employeeRow

restrict :: QueryArr Bool ()
restrict = Kleisli guard

-- Recall: guard :: Bool -> [()]

third :: (a, b, c) -> c
third (_, _, z) = z


-- # Join
--
-- Join is just restriction on an equality

managerOf :: Query (String, String)
managerOf = proc () -> do
  (employee, department, _) <- employees -< ()
  (department', manager)    <- managers  -< ()

  restrict -< department == department'

  returnA -< (employee, manager)



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
