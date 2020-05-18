module FireLink.BackEnd.TargetCodeGenerator (mapper)

where

import Data.List                                  (intercalate)
import Data.Maybe                                 (catMaybes)
import Data.Monoid                                ((<>))
import FireLink.BackEnd.CodeGenerator             (OperandType, SimpleType (..),
                                                   TAC)
import FireLink.BackEnd.RegisterAllocationProcess (Register (..),
                                                   RegisterAssignment)
import FireLink.Utils                             (bold, nocolor, red)
import TACType

import qualified Data.Map
import qualified Data.Set

tab, add, addi, li, jal, jr, la :: String
tab = replicate 4 ' '
add = tab <> "add"
addi = tab <> "addi"
li = tab <> "li"
jal = tab <> "jal"
jr = tab <> "jr"
la = tab <> "la"

syscall :: Int -> String
syscall code =
    li <> " " <> show (Register "v0") <> " " <> show code <> "\n" <> tab <> "syscall"

mapper' :: RegisterAssignment -> Data.Map.Map String String -> TAC -> String
mapper' registerAssignment stringsMap tac =
    let getValue :: OperandType -> String
        getValue (Id x)            = show $ registerAssignment Data.Map.! x
        getValue (Constant (x, _)) = x

        getStringKey :: String -> String
        getStringKey = (Data.Map.!) stringsMap

        in
    case tac of
        ThreeAddressCode Add (Just x) (Just y) (Just z) ->
            add <> " " <> getValue x  <> " " <> getValue y <> " " <> getValue z

        (ThreeAddressCode NewLabel Nothing (Just label) Nothing) ->
            show label <> ":"

        ThreeAddressCode Exit Nothing Nothing Nothing -> syscall 10

        ThreeAddressCode Call Nothing (Just l) (Just n) ->
            jal <> " " <> show l

        ThreeAddressCode Return Nothing Nothing Nothing ->
            jr <> " $ra"

        ThreeAddressCode Assign (Just x) (Just y) _ ->
            li <> " " <> getValue x <> " " <> getValue y

        ThreeAddressCode Print Nothing (Just e) Nothing ->
            case e of

                -- print "my string"
                Constant (c, StringTAC) ->
                    la <> " " <> show (Register "a0") <> " " <> getStringKey c <> "\n" <>
                    syscall 4

                -- print 111
                Constant (c, BigIntTAC) ->
                    li <> " " <> show (Register "a0") <> " " <> c <> "\n" <>
                    syscall 1

                Constant (c, SmallIntTAC) ->
                    li <> " " <> show (Register "a0") <> " " <> c <> "\n" <>
                    syscall 1

                _ -> error $ show e

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
mapper regAssignment tacs = dataSegment <> textSegment
    where
        getString :: TAC -> [String]
        getString (ThreeAddressCode _ a b c) =
            let operands = catMaybes [a, b, c]
                f op = case op of
                    Constant (s, StringTAC) -> [s]
                    _                       -> []
                in concatMap f operands

        allStrings :: [String]
        allStrings = Data.Set.toList $ Data.Set.fromList $ concatMap getString tacs

        stringsMap :: Data.Map.Map String String
        stringsMap =
            let keys = map (("string" <>) . show) [1 .. length allStrings]
                values = allStrings
                in Data.Map.fromList $ zip values keys

        textSegment :: [String]
        textSegment = ".text" : map (mapper' regAssignment stringsMap) tacs


        dataSegment :: [String]
        dataSegment = [".data"] <>
            map (\(value, key) -> key <> ": .asciiz \"" <> value <> "\"") (Data.Map.toList stringsMap)
