module SchemaArrow ( module SchemaArrow
                   , Schema.Employee(..)) where

import Opalist (listQuery, Query)
import qualified Schema

employees :: Query Schema.Employee
employees = listQuery Schema.employees

output :: Query (String, String, String, Int)
output = listQuery Schema.output

managers :: Query (String, String)
managers = listQuery Schema.managers
