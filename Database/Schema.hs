{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Schema (module Schema, module SchemaAnswer) where

import Opaleye
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import SchemaAnswer

data Employee' a b c = Employee {
    eName       :: a
  , eDepartment :: b
  , eCountry    :: c
  }
  deriving Show
$(makeAdaptorAndInstance "pEmployee" ''Employee')

type EmployeeCol = Employee' (Column PGText)
                             (Column PGText)
                             (Column PGText)

type Employee = Employee' String
                          String
                          String

employees :: Table EmployeeCol EmployeeCol
employees = Table "employee" (pEmployee Employee {
    eName       = required "name"
  , eDepartment = required "department"
  , eCountry    = required "country"
  })

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
  output  integer NOT NULL
);

CREATE TABLE manager (
  department text    NOT NULL,
  manager    text    NOT NULL
);

-}
