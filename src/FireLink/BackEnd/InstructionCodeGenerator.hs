module FireLink.BackEnd.InstructionCodeGenerator where

import           Control.Monad.RWS                  (ask, tell, unless, when)
import           FireLink.BackEnd.CodeGenerator
import           FireLink.BackEnd.ExprCodeGenerator (genBooleanComparison,
                                                     genCode',
                                                     genCodeForBooleanExpr,
                                                     genCodeForExpr, genOp2Code, genParams)
import           FireLink.FrontEnd.Grammar          (BaseExpr (..),
                                                     CodeBlock (..), Expr (..),
                                                     IfCase (..),
                                                     Instruction (..),
                                                     Program (..),
                                                     SwitchCase (..))
import qualified FireLink.FrontEnd.Grammar          as G (Id (..), Op2 (..))
import           FireLink.FrontEnd.SymTable         (wordSize, Dictionary, findSymEntryById,
                                                    DictionaryEntry (..), getUnionAttrId, findAllFunctionsAndProcedures, getCodeBlock)
import           FireLink.FrontEnd.TypeChecking     (Type (..))
import FireLink.FrontEnd.Tokens (Token (..))
import           TACType


{-
For a whole program to work, we need to generate for each function its respective
code.

The main codeblock is treated as a function, so the first instruction in our code
is to call the `main` function. That way, the `return` statement on it can behave
like procedure without any additional treatment.
-}
instance GenerateCode Program where
    genCode (Program codeblock@(CodeBlock _ maxOffset)) = do
        functions <- getFunctions <$> ask
        let allFunctions = ("_main", codeblock) : functions
        tell [ThreeAddressCode
            { tacOperand = Call
            , tacLvalue = Nothing
            , tacRvalue1 = Just $ Label "_main"
            , tacRvalue2 = Just $ Constant ("0", SmallIntT)
            }]

        mapM_ genBlock allFunctions
        where
            alignedOffset :: Int -> Int
            alignedOffset maxOffset =
                if maxOffset `mod` wordSize == 0
                then maxOffset
                else maxOffset +  wordSize - (maxOffset `mod` wordSize)

            getFunctions :: Dictionary -> [(String, CodeBlock)]
            getFunctions = map mapDictionaryToTuple . findAllFunctionsAndProcedures

            mapDictionaryToTuple :: DictionaryEntry -> (String, CodeBlock)
            mapDictionaryToTuple entry = (name entry, getCodeBlock entry)

            genBlock :: (String, CodeBlock) -> CodeGenMonad ()
            genBlock (funName, codeblock@(CodeBlock _ maxOffset)) = do
                setTempOffset $ alignedOffset maxOffset
                genLabel $ Label funName
                genCode codeblock

instance GenerateCode CodeBlock where
    genCode (CodeBlock instrs _) = mapM_ genCode instrs

instance GenerateCode Instruction where
    genCode instruction = do
        next <- newLabel
        genCodeForInstruction instruction next

genCodeForInstruction :: Instruction -> OperandType -> CodeGenMonad ()

-- Utility instructions
genCodeForInstruction (InstPrint expr) _ = return () --genCode expr
genCodeForInstruction (InstRead _) _ = return () --genCode expr

genCodeForInstruction InstReturn _ =
    tell [ThreeAddressCode
            { tacOperand = Return
            , tacLvalue = Nothing
            , tacRvalue1 = Nothing
            , tacRvalue2 = Nothing
            }]

genCodeForInstruction (InstReturnWith expr) _ = do
    operand <- genCode' expr
    tell [ThreeAddressCode
            { tacOperand = Return
            , tacLvalue = Nothing
            , tacRvalue1 = Just operand
            , tacRvalue2 = Nothing
            }]

{-
For functions/procedures calls we only generate the code for each parameter and call the function
as it was a label
-}
genCodeForInstruction (InstCall fId params) _ = do
    paramsLength <- genParams params
    funEntry <- findSymEntryById fId <$> ask
    tell [ThreeAddressCode
            { tacOperand = Call
            , tacLvalue = Nothing
            , tacRvalue1 = Just $ Label $ name funEntry
            , tacRvalue2 = Just $ Constant (show paramsLength, SmallIntT)
            }]

-- Assignments, currently supported for id assignments, structs & unions
genCodeForInstruction (InstAsig lvalue rvalue) next =
    if supportedLvalue lvalue then do
        if expType rvalue == StructLitT then assignStructLiteral lvalue rvalue
        else if expType rvalue /= TrileanT then do
            operand <- genCode' lvalue
            rValueAddress <- genCode' rvalue
            genIdAssignment operand rValueAddress
        else do
            operand <- genCode' lvalue
            trueLabel <- newLabel
            falseLabel <- newLabel
            genCodeForBooleanExpr (expAst rvalue) trueLabel falseLabel
            genLabel trueLabel
            genIdAssignment operand $ Constant ("true", TrileanT)
            genGoTo next
            genLabel falseLabel
            genIdAssignment operand $ Constant ("false", TrileanT)
            genLabel next

        -- The following code takes care of the isActive attribute for unions
        when (isUnionBExpr $ expAst lvalue) $ do
            let Expr { expAst = (Access unionExpr propId) } = lvalue
            markActiveAttrForUnion unionExpr propId

    else error $ "Lvalue currently not supported for assignments: " ++ show lvalue
    where
        supportedLvalue :: Expr -> Bool
        supportedLvalue Expr {expAst = IdExpr _ }  = True
        supportedLvalue Expr {expAst = Access _ _} = True
        supportedLvalue _                          = False

        isUnionBExpr :: BaseExpr -> Bool
        isUnionBExpr (Access Expr { expType = UnionT _ _ } _) = True
        isUnionBExpr _                                        = False

        isUnionT :: Type -> Bool
        isUnionT (UnionT _ _) = True
        isUnionT _            = False

        markActiveAttrForUnion :: Expr -> G.Id -> CodeGenMonad ()
        markActiveAttrForUnion unionExpr propId = do
            propertySymEntry <- findSymEntryById propId <$> ask
            let argPos = getUnionAttrId propertySymEntry
            unionExprOp <- genCode' unionExpr
            genIdAssignment unionExprOp $ Constant (show argPos, BigIntT)

        assignStructLiteral :: Expr -> Expr -> CodeGenMonad ()
        assignStructLiteral lvalue Expr { expAst = StructLit propAssignments } = do
            fieldsScope <- case expType lvalue of
                UnionT fieldsScope _  -> return fieldsScope
                RecordT fieldsScope _ -> return fieldsScope
            mapM_ (assignPropertyValue fieldsScope lvalue) propAssignments

            when (isUnionT $ expType lvalue) $ do
                let [(G.Id tk _, _)] = propAssignments
                markActiveAttrForUnion lvalue (G.Id tk fieldsScope)

        assignPropertyValue :: Int -> Expr -> (G.Id, Expr) -> CodeGenMonad ()
        assignPropertyValue scope lvalue (G.Id tk _, e@Expr { expType = eT }) = do
            lOperand <- genCodeForExpr eT (Access lvalue (G.Id tk scope))
            rOperand <- genCode' e
            genIdAssignment lOperand rOperand

{-
Conditional selection statement
The code-generation depends on the position of a guard in the list

If it is the last one, then its next instruction is right there,
so there is no need to make a `goto falseLabel` because there is no falseLabel

Otherwise, the next instruction of a guard block is the next instruction right
after the if whole block
-}
genCodeForInstruction (InstIf ifcases) next = do
    let initInstructions = init ifcases
    let lastInstruction = last ifcases
    mapM_ (genCodeForIfCase next False) initInstructions
    genCodeForIfCase next True lastInstruction
    genLabel next

{-
Selection by cases statement (switch)
The code-generation is similar to that of a conditional statement.

Note that the base expression is evaluated just once, in order to avoid
undesired repetition of side effects, and also as an optimization.

If we are generating the last case, the next instruction is right beneath,
so no `goto falseLabel` is generated.

Otherwise, the next instruction of a swith case is the next instruction right
after the switch whole block.

One consideration: if a default case is provided, then it's generated as a normal
switch case in which the given expr matches the base expr (basically,
a comparison of the base expression against itself is raised). This can be
further optimized to remove the unnecessary comparison.
-}
genCodeForInstruction (InstSwitch baseExpr switchCases) next = do
    bExprOperand <- genCode' baseExpr
    let initCases = init switchCases
    let lastCase = last switchCases
    mapM_ (genCodeForSwitchCase next bExprOperand False) initCases
    genCodeForSwitchCase next bExprOperand True lastCase
    genLabel next

{-
Indeterminate looping statement
Code-generation is similar as on the slides
-}
genCodeForInstruction (InstWhile guard codeblock) next = do
    (begin, trueLabel, falseLabel) <- setUpIteration next
    genLabel begin
    genCodeForBooleanExpr (expAst guard) trueLabel falseLabel
    genLabel trueLabel
    genCode codeblock
    genGoTo begin
    genLabel next

{-
Bounded looping statement
Code generation is similar to that of an indeterminate loop,
in which the guard is to check whether the iteration variable has already
reached the bound, and additional instructions are added underneath the code block
in order to successfully update the value of the iteration variable
after every iteration.
-}
genCodeForInstruction (InstFor id step bound codeblock) next = do
    (begin, trueLabel, falseLabel) <- setUpIteration next
    let idAst = IdExpr id
    idOperand <- genCodeForExpr BigIntT idAst
    boundOperand <- genCode' bound
    genLabel begin
    genBooleanComparison idOperand boundOperand trueLabel falseLabel G.Lt
    genLabel trueLabel
    genCode codeblock
    incOperand <- genIncrement idOperand step
    genIdAssignment idOperand incOperand
    genGoTo begin
    genLabel next
    where
            genIncrement :: OperandType -> Expr -> CodeGenMonad OperandType
            genIncrement idOp step = do
                stepOp <- genCode' step
                genOp2Code Add idOp stepOp

genCodeForInstruction i _ = error $ "This instruction hasn't been implemented " ++ show i

-- Aux function for iterations
setUpIteration :: OperandType -> CodeGenMonad (OperandType, OperandType, OperandType)
setUpIteration next = do
    begin <- newLabel
    trueLabel <- newLabel
    return (begin, trueLabel, next)

genCodeForIfCase :: OperandType -> Bool -> IfCase -> CodeGenMonad ()
genCodeForIfCase next isLast (GuardedCase expr codeblock) = do
    trueLabel <- if isLast then return fall else newLabel
    falseLabel <- if isLast then return next else newLabel
    genCodeForBooleanExpr (expAst expr) trueLabel falseLabel
    unless isLast $ genLabel trueLabel
    genCode codeblock
    unless isLast $ genGoTo next
    unless isLast $ genLabel falseLabel

genCodeForSwitchCase :: OperandType -> OperandType -> Bool -> SwitchCase -> CodeGenMonad ()
genCodeForSwitchCase next bExprOperand isLast sCase = do
    trueLabel <- if isLast then return fall else newLabel
    falseLabel <- if isLast then return next else newLabel
    case sCase of
        Case expr codeblock -> do
            caseExprOperand <- genCode' expr
            genBooleanComparison bExprOperand caseExprOperand trueLabel falseLabel G.Eq
            unless isLast $ genLabel trueLabel
            genCode codeblock
        DefaultCase codeblock -> do
            genBooleanComparison bExprOperand bExprOperand trueLabel falseLabel G.Eq
            unless isLast $ genLabel trueLabel
            genCode codeblock
    unless isLast $ genGoTo next
    unless isLast $ genLabel falseLabel
