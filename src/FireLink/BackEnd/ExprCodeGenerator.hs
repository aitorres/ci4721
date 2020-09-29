module FireLink.BackEnd.ExprCodeGenerator where

import           Control.Monad                  (void)
import           Control.Monad.RWS
import           Data.Char                      (ord)
import           FireLink.BackEnd.CodeGenerator
import           FireLink.FrontEnd.Grammar      (BaseExpr (..), Expr (..),
                                                 Id (..), Op1 (..), Op2 (..),
                                                 booleanOp2, comparableOp2)
import           FireLink.FrontEnd.SymTable     (Dictionary,
                                                 DictionaryEntry (..),
                                                 definedTypes, findSymEntryById,
                                                 findSymEntryByName, findWidth,
                                                 getOffset, getUnionAttrId)
import qualified FireLink.FrontEnd.SymTable     as ST (Extra (..),
                                                       extractTypeFromExtra,
                                                       sign)
import           FireLink.FrontEnd.Tokens       (Token (..))
import qualified FireLink.FrontEnd.TypeChecking as T (Type (..))
import qualified TACType                        as TAC

instance GenerateCode Expr where
    genCode = void . genCode'

genCode' :: Expr -> CodeGenMonad OperandType
genCode' e@Expr {expAst=ast@(Access struct _), expType=t} = do
    ret <- genCodeForExpr t ast

    when (isUnionT $ expType struct ) $ do
        trueLabel <- newLabel
        falseLabel <- newLabel
        let activeExpr = IsActive e
        genCodeForBooleanExpr activeExpr trueLabel falseLabel
        genLabel falseLabel
        raiseRunTimeError "RUNTIME ERROR: Trying to access inactive property of Union"
        genLabel trueLabel
    return ret
    where
        isUnionT :: T.Type -> Bool
        isUnionT (T.UnionT _ _) = True
        isUnionT _              = False
genCode' Expr {expAst=ast, expType=t} = genCodeForExpr t ast

genCodeForExpr :: T.Type -> BaseExpr -> CodeGenMonad OperandType
-- | Ids
genCodeForExpr _ (IdExpr exprId) = do
    symEntry <- findSymEntryById exprId <$> ask
    return $ TAC.Id $ TACVariable symEntry $ getOffset symEntry

-- | Property access
genCodeForExpr t (Access expr propId) = do
    let Expr { expAst = eAst, expType = eT } = expr
    TAC.Id x <- genCodeForExpr eT eAst
    let rO = getTACSymEntryOffset x
    propSymEntry <- findSymEntryById propId <$> ask
    let propOffset = getOffset propSymEntry
    return $ TAC.Id $ TACVariable propSymEntry $ propOffset + rO

genCodeForExpr _ (EvalFunc fId params) = do
    paramsLength <- genParams params
    ret <- TAC.Id <$> newtemp
    funEntry <- findSymEntryById fId <$> ask
    gen [TAC.ThreeAddressCode
            { TAC.tacOperand = TAC.Call
            , TAC.tacLvalue = Just ret
            , TAC.tacRvalue1 = Just $ TAC.Label $ name funEntry
            , TAC.tacRvalue2 = Just $ TAC.Constant (show paramsLength, BigIntTAC)
            }]
    return ret

genCodeForExpr T.TrileanT exp = do
    trueLabel <- newLabel
    falseLabel <- newLabel
    next <- newLabel
    lvalue <- TAC.Id <$> newtemp
    genCodeForBooleanExpr exp trueLabel falseLabel
    genLabel trueLabel
    genIdAssignment lvalue $ TAC.Constant ("true", TrileanTAC)
    genGoTo next
    genLabel falseLabel
    genIdAssignment lvalue $ TAC.Constant ("false", TrileanTAC)
    genLabel next
    return lvalue

genCodeForExpr _ (Op2 op lexpr rexpr) = do
    -- TODO: esto esta generando cosas tipo "n := 1 + 2"
    lId <- genCode' lexpr
    rId <- genCode' rexpr
    genOp2Code operation lId rId
    where
        operation :: TAC.Operation
        operation = mapOp2ToTacOperation op

genCodeForExpr t (IntLit n) = return $ TAC.Constant (show n, if t == T.BigIntT then BigIntTAC else SmallIntTAC)
genCodeForExpr t (FloatLit n) = return $ TAC.Constant (show n, FloatTAC)
genCodeForExpr t (CharLit c) = return $ TAC.Constant (show $ ord c, CharTAC)
genCodeForExpr t (StringLit s) = return $ TAC.Constant (s, StringTAC)

genCodeForExpr t (Caster expr newType) = do
    tempOp <- genCode' expr
    let oldType = expType expr
    lvalue <- TAC.Id <$> newtemp
    gen [TAC.ThreeAddressCode
            { TAC.tacOperand = TAC.Cast (show oldType) (show newType)
            , TAC.tacLvalue = Just lvalue
            , TAC.tacRvalue1 = Just tempOp
            , TAC.tacRvalue2 = Nothing
            }]
    return lvalue

genCodeForExpr _ (Op1 Negate expr) = do
    rId <- genCode' expr
    lvalue <- TAC.Id <$> newtemp
    gen [TAC.ThreeAddressCode
            { TAC.tacOperand = TAC.Minus
            , TAC.tacLvalue = Just lvalue
            , TAC.tacRvalue1 = Just rId
            , TAC.tacRvalue2 = Nothing
            }]
    return lvalue

{-
This implementation works because in TAC and in MIPS characters are just numbers
-}
genCodeForExpr _ (AsciiOf expr) = genCode' expr

genCodeForExpr _ (IndexAccess array index) = do
    (arrayRefOperand, _, base) <- genIndexAccess array index
    operand <- TAC.Id <$> newtemp
    gen [TAC.ThreeAddressCode
        { TAC.tacOperand = TAC.Get
        , TAC.tacLvalue = Just operand
        , TAC.tacRvalue1 = Just base
        , TAC.tacRvalue2 = Just arrayRefOperand
        }]
    return operand

genCodeForExpr _ (Size container) = genCodeForSize container

genCodeForExpr _ e = error $ "This expression hasn't been implemented " ++ show e

-- Generates code to calculate size of a collection
genCodeForSize :: Expr -> CodeGenMonad OperandType
genCodeForSize exp = case expAst exp of
    IdExpr i -> do
        idEntry <- findSymEntryById i <$> ask
        let ST.Simple entryType = ST.extractTypeFromExtra idEntry
        typeWidth entryType

-- Return type is (offset, contents, base)
genIndexAccess :: Expr -> Expr -> CodeGenMonad (OperandType, String, OperandType)
genIndexAccess array index = do
    indexOperand <- genCode' index
    genIndexAccess' array indexOperand

genIndexAccess' :: Expr -> OperandType -> CodeGenMonad (OperandType, String, OperandType)
genIndexAccess' array indexOperand = case expAst array of
    IdExpr idArray -> do
        idEntry <- findSymEntryById idArray <$> ask
        contents <- getContents $ ST.extractTypeFromExtra idEntry
        width <- typeWidth contents
        resultAddress <- TAC.Id <$> newtemp
        gen [TAC.ThreeAddressCode
                { TAC.tacOperand = TAC.Mult
                , TAC.tacLvalue = Just resultAddress
                , TAC.tacRvalue1 = Just indexOperand
                , TAC.tacRvalue2 = Just width
                }]
        return (resultAddress, contents, TAC.Id $ TACVariable idEntry (getOffset idEntry))
    IndexAccess array' index' -> do
        (offset, contents', base) <- genIndexAccess array' index'
        contents <- getContents $ ST.Simple contents'
        width <- typeWidth contents
        t <- TAC.Id <$> newtemp
        gen [TAC.ThreeAddressCode
                { TAC.tacOperand = TAC.Add
                , TAC.tacLvalue = Just t
                , TAC.tacRvalue1 = Just offset
                , TAC.tacRvalue2 = Just indexOperand
                }]
        resultAddress <- TAC.Id <$> newtemp
        gen [TAC.ThreeAddressCode
                { TAC.tacOperand = TAC.Mult
                , TAC.tacLvalue = Just resultAddress
                , TAC.tacRvalue1 = Just t
                , TAC.tacRvalue2 = Just width
                }]
        return (resultAddress, contents, base)
    e -> error $ "unsupported index access for expression " ++ show e
    where
        getContents :: ST.Extra -> CodeGenMonad String
        getContents (ST.CompoundRec _ _ s@(ST.Simple contentsType)) = return contentsType
        getContents (ST.Compound ">-miracle" _) = return ST.sign

        -- Since this function is only used with arrays, this call should never have as an argument a definedType
        getContents (ST.Simple t) =
            ST.extractTypeFromExtra . findSymEntryByName t <$> ask >>= getContents
        getContents a = error $ "error processing " ++ show a


genParams :: [Expr] -> CodeGenMonad Int
genParams params = do
    operands <- mapM genCode' params
    gen $ map createParam operands
    return $ length params
    where
        createParam :: OperandType -> TAC
        createParam o = TAC.ThreeAddressCode
            { TAC.tacOperand = TAC.Param
            , TAC.tacLvalue = Nothing
            , TAC.tacRvalue1 = Just o
            , TAC.tacRvalue2 = Nothing
            }


genOp2Code :: TAC.Operation -> OperandType -> OperandType -> CodeGenMonad OperandType
genOp2Code operation lId rId = do
    lvalue <- TAC.Id <$> newtemp
    tell [TAC.ThreeAddressCode
            { TAC.tacOperand = operation
            , TAC.tacLvalue = Just lvalue
            , TAC.tacRvalue1 = Just lId
            , TAC.tacRvalue2 = Just rId
            }]
    return lvalue

genCodeForBooleanExpr :: BaseExpr -> OperandType -> OperandType -> CodeGenMonad ()

genCodeForBooleanExpr expr trueLabel falseLabel = case expr of
    -- Boolean literals
    TrueLit -> unless (isFall trueLabel) $ genGoTo trueLabel
    FalseLit -> unless (isFall falseLabel) $ genGoTo falseLabel

    -- Boolean negation
    Op1 Not expr -> genCodeForBooleanExpr (expAst expr) falseLabel trueLabel

    IdExpr _ -> do
        symEntry <- genCodeForExpr T.TrileanT expr
        let isTrueNotFall = not $ isFall trueLabel
        let isFalseNotFall = not $ isFall falseLabel
        if isTrueNotFall && isFalseNotFall then do
            gen [TAC.ThreeAddressCode
                    { TAC.tacOperand = TAC.Eq
                    , TAC.tacLvalue = Just symEntry
                    , TAC.tacRvalue1 = Just $ TAC.Constant ("true", TrileanTAC)
                    , TAC.tacRvalue2 = Just trueLabel
                    }]
            genGoTo falseLabel
        else if isTrueNotFall then
            gen [TAC.ThreeAddressCode
                    { TAC.tacOperand = TAC.Eq
                    , TAC.tacLvalue = Just symEntry
                    , TAC.tacRvalue1 = Just $ TAC.Constant ("true", TrileanTAC)
                    , TAC.tacRvalue2 = Just trueLabel
                    }]
        else when isFalseNotFall (gen [TAC.ThreeAddressCode
                    { TAC.tacOperand = TAC.Eq
                    , TAC.tacLvalue = Just symEntry
                    , TAC.tacRvalue1 = Just $ TAC.Constant ("false", TrileanTAC)
                    , TAC.tacRvalue2 = Just falseLabel
                    }])
    -- Boolean comparation
    Op2 op lhs rhs | op `elem` comparableOp2 -> do
        leftExprId <- genCode' lhs
        rightExprId <- genCode' rhs
        genBooleanComparison leftExprId rightExprId trueLabel falseLabel op

    -- Conjunction and disjunction
    Op2 op lhs rhs | op `elem` booleanOp2 -> do
        lhsTrueLabel <-
            -- for `or` we need to generate a new `true` label if the current one is `fall`
            if op == Or then
                (if isFall trueLabel then newLabel else return trueLabel)
            -- for `and` we just need to `fall`
            else
                return fall
        lhsFalseLabel <-
            if op == Or then
                return fall
            else
                (if isFall falseLabel then newLabel else return falseLabel)
        let rhsTrueLabel = trueLabel
        let rhsFalseLabel = falseLabel
        if op == Or then do
            genCodeForBooleanExpr (expAst lhs) lhsTrueLabel lhsFalseLabel
            genCodeForBooleanExpr (expAst rhs) rhsTrueLabel rhsFalseLabel
            when (isFall trueLabel) (genLabel lhsTrueLabel)
        else do
            genCodeForBooleanExpr (expAst lhs) lhsTrueLabel lhsFalseLabel
            genCodeForBooleanExpr (expAst rhs) rhsTrueLabel rhsFalseLabel
            when (isFall falseLabel) (genLabel lhsFalseLabel)
    (EvalFunc _ _) -> do
        resultOperand <- genCodeForExpr T.TrileanT expr
        genBooleanComparison resultOperand (TAC.Constant ("true", TrileanTAC)) trueLabel falseLabel Eq

    IsActive Expr { expAst = Access unionExpr propId } -> do
        -- Get union's base direction (where real is_active is stored)
        realArgOperand <- genCode' unionExpr

        -- Get received argument's attr number for comparison
        propSymEntry <- findSymEntryById propId <$> ask
        let propArgPos = getUnionAttrId propSymEntry

        -- Assign received argument's attr number to a new temp
        propArgOperand <- TAC.Id <$> newtemp
        genIdAssignment propArgOperand $ TAC.Constant (show propArgPos, BigIntTAC)

        -- Gen boolean comparison
        genBooleanComparison propArgOperand realArgOperand trueLabel falseLabel Eq

    e -> error $ "Unexpected boolean expression: No pattern on genCodeForBooleanExpr for  " ++ show e

-- Used in boolean expr generation as well as switch case generation
genBooleanComparison :: OperandType -> OperandType -> OperandType -> OperandType -> Op2 -> CodeGenMonad ()
genBooleanComparison leftExprId rightExprId trueLabel falseLabel op = do
    let isTrueNotFall = not $ isFall trueLabel
    let isFalseNotFall = not $ isFall falseLabel
    if isTrueNotFall && isFalseNotFall then do
        gen [TAC.ThreeAddressCode
                { TAC.tacOperand = mapOp2ToTacOperation op
                , TAC.tacLvalue = Just leftExprId
                , TAC.tacRvalue1 = Just rightExprId
                , TAC.tacRvalue2 = Just trueLabel
                }]
        genGoTo falseLabel
    else if isTrueNotFall then
        gen [TAC.ThreeAddressCode
                { TAC.tacOperand = mapOp2ToTacOperation op
                , TAC.tacLvalue = Just leftExprId
                , TAC.tacRvalue1 = Just rightExprId
                , TAC.tacRvalue2 = Just trueLabel
                }]
    else when isFalseNotFall (gen [TAC.ThreeAddressCode
                { TAC.tacOperand = complement $ mapOp2ToTacOperation op
                , TAC.tacLvalue = Just leftExprId
                , TAC.tacRvalue1 = Just rightExprId
                , TAC.tacRvalue2 = Just falseLabel
                }])

mapOp2ToTacOperation :: Op2 -> TAC.Operation
mapOp2ToTacOperation op = case op of
    Lt        -> TAC.Lt
    Gt        -> TAC.Gt
    Lte       -> TAC.Lte
    Gte       -> TAC.Gte
    Eq        -> TAC.Eq
    Neq       -> TAC.Neq
    Add       -> TAC.Add
    Substract -> TAC.Sub
    Multiply  -> TAC.Mult
    Divide    -> TAC.Div
    Mod       -> TAC.Mod

complement :: TAC.Operation -> TAC.Operation
complement TAC.Lt  = TAC.Gte
complement TAC.Lte = TAC.Gt
complement TAC.Gt  = TAC.Lte
complement TAC.Gte = TAC.Lt
complement TAC.Eq  = TAC.Neq
complement TAC.Neq = TAC.Eq
