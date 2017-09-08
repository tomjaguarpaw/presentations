{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SchemaAnswer where

import Opaleye
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)

data EmployeeOutput' a b c d = EmployeeOutput {
    oName    :: a
  , oDay     :: b
  , oCountry :: c
  , oOutput  :: d
  }
  deriving Show
$(makeAdaptorAndInstance "pEmployeeOutput" ''EmployeeOutput')

data Manager' a b = Manager {
    mDepartment :: a
  , mManager    :: b
  }
  deriving Show
$(makeAdaptorAndInstance "pManager" ''Manager')

type EmployeeOutputCol =
  EmployeeOutput' (Column PGText)
                  (Column PGText)
                  (Column PGText)
                  (Column PGInt8)

type ManagerCol =
  Manager' (Column PGText)
           (Column PGText)

type Manager =
  Manager' String
           String

output :: Table EmployeeOutputCol EmployeeOutputCol
output = Table "output" (pEmployeeOutput EmployeeOutput {
    oName    = required "name"
  , oDay     = required "day"
  , oCountry = required "country"
  , oOutput  = required "output"
  })

managers :: Table ManagerCol ManagerCol
managers = Table "manager" (pManager Manager {
    mDepartment = required "department"
  , mManager    = required "manager"
  })
