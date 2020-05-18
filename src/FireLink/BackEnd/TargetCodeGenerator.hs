module FireLink.BackEnd.TargetCodeGenerator ()

where

import Data.List                                  (intercalate)
import Data.Maybe                                 (catMaybes)
import Data.Monoid                                ((<>))
import FireLink.BackEnd.CodeGenerator             (TAC)
import FireLink.BackEnd.RegisterAllocationProcess (Register (..),
                                                   RegisterAssignment)
import TACType

add, addi :: String
add = "add"
addi = "addi"


mapper :: RegisterAssignment -> TAC -> String
mapper = undefined
