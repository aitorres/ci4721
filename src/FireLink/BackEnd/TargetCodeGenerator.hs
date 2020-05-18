module FireLink.BackEnd.TargetCodeGenerator (mapper)

where

import Data.List                                  (intercalate)
import Data.Maybe                                 (catMaybes)
import Data.Monoid                                ((<>))
import FireLink.BackEnd.CodeGenerator             (OperandType, TAC)
import FireLink.BackEnd.RegisterAllocationProcess (Register (..),
                                                   RegisterAssignment)
import FireLink.Utils                             (bold, nocolor, red)
import TACType

import qualified Data.Map

tab, add, addi, syscall, li, jal, jr :: String
tab = replicate 4 ' '
add = tab <> "add"
addi = tab <> "addi"
syscall = tab <> "syscall"
li = tab <> "li"
jal = tab <> "jal"
jr = tab <> "jr"

mapper' :: RegisterAssignment -> TAC -> String
mapper' registerAssignment tac =
    let getValue :: OperandType -> String
        getValue (Id x)            = show $ registerAssignment Data.Map.! x
        getValue (Constant (x, _)) = x
        in
    case tac of
        ThreeAddressCode Add (Just x) (Just y) (Just z) ->
            add <> " " <> getValue x  <> " " <> getValue y <> " " <> getValue z

        t@(ThreeAddressCode NewLabel Nothing (Just label) Nothing) -> show t

        ThreeAddressCode Exit Nothing Nothing Nothing ->
            li <> " " <> show (Register "v0") <> " 10" <> "\n" <> syscall

        ThreeAddressCode Call Nothing (Just l) (Just n) ->
            jal <> " " <> show l

        ThreeAddressCode Return Nothing Nothing Nothing ->
            jr <> " $ra"

        ThreeAddressCode Assign (Just x) (Just y) _ ->
            li <> " " <> getValue x <> " " <> getValue y

        _ -> red <> bold <> show tac <> " # not implemented yet" <> nocolor

        -- ThreeAddressCode Store (Just (Id v)) Nothing Nothing ->
        -- ThreeAddressCode Load (Just (Id v)) Nothing Nothing ->
        -- ThreeAddressCode Add (Just x) (Just y) (Just z) ->
        -- ThreeAddressCode Minus (Just x) (Just y) Nothing ->
        -- ThreeAddressCode Sub (Just x) (Just y) (Just z) ->
        -- ThreeAddressCode Mult (Just x) (Just y) (Just z) ->
        -- ThreeAddressCode Div (Just x) (Just y) (Just z) ->
        -- ThreeAddressCode Mod (Just x) (Just y) (Just z) ->
        -- ThreeAddressCode (Cast _ toType) (Just x) (Just y) _ ->
        -- ThreeAddressCode Not (Just x) (Just y) _ ->
        -- ThreeAddressCode And (Just x) (Just y) (Just z) ->
        -- ThreeAddressCode Or (Just x) (Just y) (Just z) ->
        -- ThreeAddressCode GoTo Nothing Nothing (Just label) ->
        -- ThreeAddressCode GoTo Nothing Nothing Nothing ->
        -- ThreeAddressCode If Nothing (Just b) (Just label) ->
        -- ThreeAddressCode If Nothing (Just b) Nothing ->
        -- ThreeAddressCode Eq (Just x) (Just y) (Just label) ->
        -- ThreeAddressCode Neq (Just x) (Just y) (Just label) ->
        -- ThreeAddressCode Lt (Just x) (Just y) (Just label) ->
        -- ThreeAddressCode Gt (Just x) (Just y) (Just label) ->
        -- ThreeAddressCode Lte (Just x) (Just y) (Just label) ->
        -- ThreeAddressCode Gte (Just x) (Just y) (Just label) ->
        -- ThreeAddressCode Get (Just x) (Just y) (Just i) ->
        -- ThreeAddressCode Set (Just x) (Just i) (Just y) ->
        -- ThreeAddressCode NewLabel Nothing (Just label) Nothin ->
        -- ThreeAddressCode New (Just x) (Just size) Nothing ->
        -- ThreeAddressCode Free Nothing (Just addr) Nothing ->
        -- ThreeAddressCode Ref (Just x) (Just y) Nothing ->
        -- ThreeAddressCode Param Nothing (Just p) Nothing ->
        -- ThreeAddressCode Call Nothing (Just l) (Just n) ->
        -- ThreeAddressCode Call (Just t) (Just l) (Just n) ->
        -- ThreeAddressCode Read Nothing (Just e) Nothing ->
        -- ThreeAddressCode Print Nothing (Just e) Nothing ->
        -- ThreeAddressCode Return Nothing Nothing Nothing ->
        -- ThreeAddressCode Return Nothing (Just t) Nothing ->
        -- ThreeAddressCode Exit Nothing Nothing Nothing ->
        -- ThreeAddressCode Abort Nothing Nothing Nothing ->


mapper :: RegisterAssignment -> [TAC] -> [String]
mapper = map . mapper'
