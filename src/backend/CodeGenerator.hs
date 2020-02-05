module CodeGenerator where

import Control.Monad.RWS (RWST(..), get, put, tell)
import TACType
import TypeChecking
import SymTable (DictionaryEntry(..), Dictionary(..))

data CodeGenState = CodeGenState
    { cgsNextLabel :: !Int
    , cgsNextTemp :: !Int
    }

initialState :: CodeGenState
initialState = CodeGenState {cgsNextTemp = 0, cgsNextLabel = 0}

data TACSymEntry = TACTemporal String | TACVariable DictionaryEntry

instance SymEntryCompatible TACSymEntry where
    getSymID (TACTemporal s) = s
    getSymID (TACVariable entry) = name entry

instance Show TACSymEntry where
    show = getSymID

type TAC = ThreeAddressCode TACSymEntry Type
type OperandType = Operand TACSymEntry Type

type CodeGenMonad = RWST Dictionary [TAC] CodeGenState IO

newtemp :: CodeGenMonad TACSymEntry
newtemp = do
    state@CodeGenState {cgsNextTemp = temp} <- get
    put $ state{cgsNextTemp = temp + 1}
    return $ TACTemporal $ "_t" ++ show temp

newLabel :: CodeGenMonad (Operand a b)
newLabel = do
    state@CodeGenState {cgsNextLabel = label} <- get
    put $ state{cgsNextLabel = label + 1}
    return $ Label label

genGoTo :: OperandType -> CodeGenMonad ()
genGoTo label = tell [ThreeAddressCode
            { tacOperand = GoTo
            , tacLvalue = Nothing
            , tacRvalue1 = Nothing
            , tacRvalue2 = Just label
            }]

genLabel :: OperandType -> CodeGenMonad ()
genLabel label = tell [ThreeAddressCode
                            { tacOperand = NewLabel
                            , tacLvalue = Nothing
                            , tacRvalue1 = Just label
                            , tacRvalue2 = Nothing
                            }]


class GenerateCode a where
    genCode :: a -> CodeGenMonad ()
