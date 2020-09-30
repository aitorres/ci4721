module FireLink.BackEnd.TargetCodeGenerator (mapper)

where

import Data.List                                  (intercalate, isInfixOf)
import Data.Maybe                                 (catMaybes)
import Data.Monoid                                ((<>))
import FireLink.BackEnd.CodeGenerator             (OperandType, SimpleType (..),
                                                   TAC, TACSymEntry (..))
import FireLink.BackEnd.RegisterAllocationProcess (Register (..),
                                                   RegisterAssignment)
import FireLink.Utils                             (bold, nocolor, red)
import TACType

import qualified FireLink.FrontEnd.SymTable as ST
import qualified Data.Map
import qualified Data.Set

-- Operations
tab, add, mul, i_div, sub, zero :: String
tab = replicate 4 ' '
add = tab <> "add"
mul = tab <> "mul"
sub = tab <> "sub"
i_div = tab <> "div" -- `div` was ambiguous
zero = "$zero"

-- Jumps & Memory access
goto, li, lis, jal, jr, la, move, mfhi, mflo :: String
goto = tab <> "j"
li = tab <> "li"
lis = tab <> "li.s"
jal = tab <> "jal"
jr = tab <> "jr"
la = tab <> "la"
move = tab <> "move"
mfhi = tab <> "mfhi"
mflo = tab <> "mflo"

syscall :: Int -> String
syscall code =
    li <> " " <> show (Register "v0") <> " " <> show code <> "\n" <> tab <> "syscall"

simpleTypeFromDictEntry :: ST.DictionaryEntry -> SimpleType
simpleTypeFromDictEntry dE =
    let typeExtra = ST.extractTypeFromExtra dE
    in  case typeExtra of
        ST.Simple s ->
            if s == ST.smallHumanity then SmallIntTAC
            else if s == ST.humanity then BigIntTAC
            else if s == ST.hollow then FloatTAC
            else if s == ST.sign then CharTAC
            else if s == ST.bonfire then TrileanTAC
            else if s == ST.void then BigIntTAC -- TODO: is this right? do we even have pointers?
            else if isInfixOf "_alias_" (show s) then error "I specifically don't know how to recover this info sox"
            else error $ "I'm sorry, this simple type is not supported :-( " ++ show s

        ST.Compound _ _ -> StringTAC

        _ -> error "You're trying to operate on an unsupported type :-("

mapper' :: RegisterAssignment -> Data.Map.Map String String -> TAC -> String
mapper' registerAssignment stringsMap tac =
    let getValue :: OperandType -> String
        getValue (Id x)            = show $ registerAssignment Data.Map.! x
        getValue (Constant (x, TrileanTAC)) = case x of
            "true" -> "1"
            "false" -> "0"
            _ -> "Ooopsie! I am yet undiscovered!"
        getValue (Constant (x, _)) = x

        getStringKey :: String -> String
        getStringKey = (Data.Map.!) stringsMap

        liToReg :: String -> String -> String
        liToReg r c = li <> " " <> r <> " " <> c <> "\n"

        in
    case tac of
        -- ?INFO: Works for int, not float
        ThreeAddressCode Add (Just x) (Just y) (Just z) ->
            add <> " " <> getValue x  <> " " <> getValue y <> " " <> getValue z

        -- ?INFO: Works for int, not float
        ThreeAddressCode Sub (Just x) (Just y) (Just z) ->
            sub <> " " <> getValue x  <> " " <> getValue y <> " " <> getValue z

        ThreeAddressCode Minus (Just x) (Just y) Nothing ->
            case y of
                -- ?INFO: Works for int, not float
                Constant (c, _) ->
                    sub <> " " <> getValue x <> " " <> zero <> " " <> c

                -- always integer
                Id (TACTemporal _ _) ->
                    sub <> " " <> getValue x <> " " <> zero <> " " <> getValue y

                -- ?INFO: Works for int, not float
                Id (TACVariable _ _) ->
                    sub <> " " <> getValue x <> " " <> zero <> " " <> getValue y

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

                -- assuming it's int
                Constant (c, CharTAC) ->
                    liToReg (getValue x) (getValue y)

                Constant (c, TrileanTAC) ->
                    case c of
                        "true" -> liToReg (getValue x) "1"
                        "false" -> liToReg (getValue x) "0"
                        _ -> error "I still live in a binary world :-("

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
                    liToReg "$a0" c <>
                    syscall 11

                -- print 111
                Constant (c, BigIntTAC) ->
                    liToReg "$a0" c <>
                    syscall 1

                Constant (c, SmallIntTAC) ->
                    liToReg "$a0" c <>
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

                -- this whole thing is still e
                Id (TACVariable x _) ->
                    case simpleTypeFromDictEntry x of
                        BigIntTAC ->
                            move <> " $a0 " <> (getValue e) <> "\n" <>
                            syscall 1

                        SmallIntTAC ->
                            move <> " $a0 " <> (getValue e) <> "\n" <>
                            syscall 1

                        -- assume int since it's either 1 or 0
                        TrileanTAC ->
                            move <> " $a0 " <> (getValue e) <> "\n" <>
                            syscall 1

                        CharTAC ->
                            move <> " $a0 " <> (getValue e) <> "\n" <>
                            syscall 11

                        StringTAC ->
                            move <> " " <> show (Register "a0") <> " " <> getValue e <> "\n" <>
                            syscall 4

                        _ -> red <> bold <> "# not implemented yet" <> nocolor

        ThreeAddressCode Read Nothing (Just e@(Id (TACVariable x _))) Nothing ->
            case simpleTypeFromDictEntry x of
                BigIntTAC ->
                    syscall 5 <> "\n" <>
                    move <> " " <> getValue e <> " " <> "$v0"

                SmallIntTAC ->
                    syscall 5 <> "\n" <>
                    move <> " " <> getValue e <> " " <> "$v0"

                CharTAC ->
                    syscall 5 <> "\n" <>
                    move <> " " <> getValue e <> " " <> "$v0"

                _ -> red <> bold <> "# not implemented yet" <> nocolor

        -- TODO: check this thing
        ThreeAddressCode (Cast _ _) a@(Just x) b@(Just y) _ ->
            mapper' registerAssignment stringsMap (ThreeAddressCode Assign a b Nothing)

        -- ?INFO: ints only for now
        ThreeAddressCode Mult (Just x) (Just y) (Just z) ->
            mul <> " " <> getValue x <> " " <> getValue y <> " " <> getValue z <> "\n"

        -- ?INFO: ints only for now
        ThreeAddressCode Div (Just x) (Just y) (Just z) ->
            i_div <> " " <> getValue y <> " " <> getValue z <> "\n" <>
            mflo <> " " <> getValue x

        -- ?INFO: ints only for now
        ThreeAddressCode Mod (Just x) (Just y) (Just z) ->
            i_div <> " " <> getValue y <> " " <> getValue z <> "\n" <>
            mfhi <> " " <> getValue x

        -- ThreeAddressCode Store (Just (Id v)) Nothing Nothing ->
        -- ThreeAddressCode Load (Just (Id v)) Nothing Nothing ->
        -- ThreeAddressCode Not (Just x) (Just y) _ ->
        -- ThreeAddressCode And (Just x) (Just y) (Just z) ->
        -- ThreeAddressCode Or (Just x) (Just y) (Just z) ->
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
        -- ThreeAddressCode Return Nothing (Just t) Nothing ->

        ThreeAddressCode GoTo Nothing Nothing (Just label) ->
            goto <> " " <> show label

        -- exit (success)
        ThreeAddressCode Exit _ _ _ ->
            syscall 10

        -- exit (failure, exit code = 1)
        ThreeAddressCode Abort _ _ _ ->
            liToReg "$a0" "1" <>
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
        textSegment = ".text\nmain:" : map (\t -> "# " <> (show t) <> "\n" <> (mapper' regAssignment stringsMap t) <> "\n") tacs

        dataSegment :: [String]
        dataSegment = [".data"] <>
            map (\(value, key) -> key <> ": .asciiz \"" <> value <> "\"") (Data.Map.toList stringsMap)
