module SchemaArrow ( module SchemaArrow
                   , Schema.Employee(..)
                   , Schema.EmployeeOutput(..)
                   , Schema.Manager(..)) where

import Opalist (listQuery, Query)
import qualified Schema

employees :: Query Schema.Employee
employees = listQuery Schema.employees

output :: Query Schema.EmployeeOutput
output = listQuery Schema.output

managers :: Query Schema.Manager
managers = listQuery Schema.managers
