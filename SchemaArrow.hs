module SchemaArrow where

import Opalist (listQuery, Query)
import qualified Schema

employees :: Query (String, String, String)
employees = listQuery Schema.employees

output :: Query (String, String, String, Int)
output = listQuery Schema.output

managers :: Query (String, String)
managers = listQuery Schema.managers
