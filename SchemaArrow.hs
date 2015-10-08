module SchemaArrow ( module SchemaArrow
                   , Schema.Employee(..)
                   , Schema.EmployeeOutput(..)) where

import Opalist (listQuery, Query)
import qualified Schema

employees :: Query Schema.Employee
employees = listQuery Schema.employees

output :: Query Schema.EmployeeOutput
output = listQuery Schema.output

managers :: Query (String, String)
managers = listQuery Schema.managers
