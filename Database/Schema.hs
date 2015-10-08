{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Schema ( module Schema
              , SchemaAnswer.EmployeeOutput'(..)
              , SchemaAnswer.EmployeeOutputCol
              , output
              , SchemaAnswer.Manager'(..)
              , SchemaAnswer.ManagerCol
              , SchemaAnswer.Manager
              , managers
              ) where

import Opaleye
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import SchemaAnswer

-- Make a record type with type variables for each field 
data Employee' a b c = Employee {
    eName       :: a
  , eDepartment :: b
  , eCountry    :: c
  }
  deriving Show
-- Add this little bit of Template Haskell
$(makeAdaptorAndInstance "pEmployee" ''Employee')

-- Add a type synonym full of columns
type EmployeeCol = Employee' (Column PGText)
                             (Column PGText)
                             (Column PGText)

-- Add a type synonym full of Haskell types
type Employee = Employee' String
                          String
                          String

employees :: Table EmployeeCol EmployeeCol
employees = Table "employee" (pEmployee Employee {
    eName       = required "name"
  , eDepartment = required "department"
  , eCountry    = required "country"
  })

{- Exercise: Implement definitions for the output and manager table
   based on the definitions below.  NB the Haskell type for int8 is
   'Column PGInt8'.
-}

{-

CREATE TABLE employee (
  name       text NOT NULL,
  department text NOT NULL,
  country    text NOT NULL
);

CREATE TABLE output (
  name    text    NOT NULL,
  day     text    NOT NULL,
  country text    NOT NULL,
  output  int8    NOT NULL
);

CREATE TABLE manager (
  department text    NOT NULL,
  manager    text    NOT NULL
);

-}
