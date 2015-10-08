{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Schema where

import Opaleye
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)

data Employee' a b c = Employee {
    eName       :: a
  , eDepartment :: b
  , eCountry    :: c
  }
  deriving Show
$(makeAdaptorAndInstance "pEmployee" ''Employee')

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

type EmployeeCol = Employee' (Column PGText)
                             (Column PGText)
                             (Column PGText)

type EmployeeOutputCol =
  EmployeeOutput' (Column PGText)
                  (Column PGText)
                  (Column PGText)
                  (Column PGInt4)

type ManagerCol =
  Manager' (Column PGText)
           (Column PGText)

employees :: Table EmployeeCol EmployeeCol
employees = Table "employee" (pEmployee Employee {
    eName       = required "name"
  , eDepartment = required "department"
  , eCountry    = required "country"
  })

output :: Table EmployeeOutputCol EmployeeOutputCol
output = Table "output" (pEmployeeOutput EmployeeOutput {
    oName    = required "name"
  , oDay     = required "day"
  , oCountry = required "country"
  , oOutput  = required "output"
  })

managers :: Table ManagerCol ManagerCol
managers = Table "output" (pManager Manager {
    mDepartment = required "department"
  , mManager    = required "manager"
  })
