module FireLink.BackEnd.TargetCodeGenerator (mapper)

where

import Data.List                                  (intercalate)
import Data.Maybe                                 (catMaybes)
import Data.Monoid                                ((<>))
import FireLink.BackEnd.CodeGenerator             (OperandType, SimpleType (..),
                                                   TAC, TACSymEntry (..))
import FireLink.BackEnd.RegisterAllocationProcess (Register (..),
                                                   RegisterAssignment)
import FireLink.Utils                             (bold, nocolor, red)
import TACType

import qualified Data.Map
import qualified Data.Set

tab, add, addi, sub, subs, li, lis, jal, jr, la, move, zero :: String
tab = replicate 4 ' '
add = tab <> "add"
addi = tab <> "addi"
sub = tab <> "sub"
subs = tab <> "sub.s"
li = tab <> "li"
lis = tab <> "li.s"
jal = tab <> "jal"
jr = tab <> "jr"
la = tab <> "la"
move = tab <> "move"
zero = "$zero"

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

        liToReg :: String -> String -> String
        liToReg r c = li <> " " <> r <> " " <> c <> "\n"

        in
    case tac of
        ThreeAddressCode Add (Just x) (Just y) (Just z) ->
            add <> " " <> getValue x  <> " " <> getValue y <> " " <> getValue z

        ThreeAddressCode Minus (Just x) (Just y) Nothing ->
            case y of

                Constant (c, BigIntTAC) ->
                    sub <> " " <> getValue x <> " " <> zero <> " " <> c

                Constant (c, SmallIntTAC) ->
                    sub <> " " <> getValue x <> " " <> zero <> " " <> c

                Constant (c, FloatTAC) ->
                    subs <> " " <> getValue x <> " " <> zero <> " " <> c

                -- always integer
                Id (TACTemporal _ _) ->
                    sub <> " " <> getValue x <> " " <> zero <> " " <> getValue y

                -- Id (TACVariable)

        (ThreeAddressCode NewLabel Nothing (Just label) Nothing) ->
            show label <> ":"

        ThreeAddressCode Call Nothing (Just l) (Just n) ->
            jal <> " " <> show l

        ThreeAddressCode Return Nothing Nothing Nothing ->
            jr <> " $ra"

        ThreeAddressCode Assign (Just x) (Just y) _ ->
            case y of
                Constant (c, BigIntTAC) ->
                    liToReg (getValue x) (getValue y)

                Constant (c, SmallIntTAC) ->
                    liToReg (getValue x) (getValue y)

                Constant (c, FloatTAC) ->
                    lis <> " " <> (getValue x) <> " " <> (getValue y)

                -- TODO: other assignments?

                -- TODO: add support for float assignments
                _ ->
                    move <> " " <> (getValue x) <> " " <> (getValue y)

        ThreeAddressCode Print Nothing (Just e) Nothing ->
            case e of

                -- print "my string"
                Constant (c, StringTAC) ->
                    la <> " " <> show (Register "a0") <> " " <> getStringKey c <> "\n" <>
                    syscall 4

                -- print 'c'
                Constant (c, CharTAC) ->
                    liToReg "a0" c <>
                    syscall 11

                -- print 111
                Constant (c, BigIntTAC) ->
                    liToReg "a0" c <>
                    syscall 1

                Constant (c, SmallIntTAC) ->
                    liToReg "a0" c <>
                    syscall 1

                -- print 1.5
                -- this would use syscall 2
                Constant (c, FloatTAC) ->
                    red <> bold <> "# print " <> c <> " not implemented yet" <> nocolor

                -- we assume for the moment that if we want to print a temporal we will print
                -- an integer
                Id (TACTemporal tempId _) ->
                    move <> " " <> show (Register "a0") <> " " <> getValue e <> "\n" <>
                    syscall 1

                Id _ -> red <> bold <> "# not implemented yet" <> nocolor

        -- ThreeAddressCode Store (Just (Id v)) Nothing Nothing ->
        -- ThreeAddressCode Load (Just (Id v)) Nothing Nothing ->
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
        -- ThreeAddressCode New (Just x) (Just size) Nothing ->
        -- ThreeAddressCode Free Nothing (Just addr) Nothing ->
        -- ThreeAddressCode Ref (Just x) (Just y) Nothing ->
        -- ThreeAddressCode Param Nothing (Just p) Nothing ->
        -- ThreeAddressCode Call Nothing (Just l) (Just n) ->
        -- ThreeAddressCode Call (Just t) (Just l) (Just n) ->
        -- ThreeAddressCode Read Nothing (Just e) Nothing ->
        -- ThreeAddressCode Return Nothing (Just t) Nothing ->

        -- exit (success)
        ThreeAddressCode Exit _ _ _ ->
            syscall 10

        -- exit (failure, exit code = 1)
        ThreeAddressCode Abort _ _ _ ->
            liToReg "a0" "1" <>
            syscall 17

        _ ->  "# " <> red <> bold <> show tac <> " not implemented yet" <> nocolor

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
        textSegment = ".text\nmain:" : map (\t -> "# " <> (show t) <> "\n" <> (mapper' regAssignment stringsMap t)) tacs


        dataSegment :: [String]
        dataSegment = [".data"] <>
            map (\(value, key) -> key <> ": .asciiz \"" <> value <> "\"") (Data.Map.toList stringsMap)
