module FireLink.BackEnd.InstructionCodeGenerator where

import           Control.Monad.RWS                  (ask, unless, when)
import           Data.Maybe                         (fromJust)
import           FireLink.BackEnd.CodeGenerator
import           FireLink.BackEnd.ExprCodeGenerator (genBooleanComparison,
                                                     genCode',
                                                     genCodeForBooleanExpr,
                                                     genCodeForExpr,
                                                     genCodeForSize,
                                                     genIndexAccess,
                                                     genIndexAccess',
                                                     genOp2Code, genParams)
import           FireLink.FrontEnd.Grammar          (BaseExpr (..),
                                                     CodeBlock (..), Expr (..),
                                                     IfCase (..),
                                                     Instruction (..),
                                                     Program (..),
                                                     SwitchCase (..))
import qualified FireLink.FrontEnd.Grammar          as G (Id (..), Op2 (..))
import           FireLink.FrontEnd.SymTable         (Dictionary,
                                                     DictionaryEntry (..),
                                                     extractTypeFromExtra,
                                                     findAllFunctionsAndProcedures,
                                                     findSymEntryById,
                                                     findSymEntryByName,
                                                     findWidth, getCodeBlock,
                                                     getOffset, getUnionAttrId,
                                                     wordSize)
import qualified FireLink.FrontEnd.SymTable         as ST (Extra (..),
                                                           definedTypes, sign)
import           FireLink.FrontEnd.Tokens           (Token (..))
import           FireLink.FrontEnd.TypeChecking     (Type (..),
                                                     getTypeFromContainer)
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
        gen [ThreeAddressCode
            { tacOperand = Call
            , tacLvalue = Nothing
            , tacRvalue1 = Just $ Label "_main"
            , tacRvalue2 = Just $ Constant ("0", SmallIntTAC)
            }, ThreeAddressCode
            { tacOperand = Exit
            , tacLvalue = Nothing
            , tacRvalue1 = Nothing
            , tacRvalue2 = Nothing
            }]
        gen [ThreeAddressCode
            { tacOperand = Exit
            , tacLvalue = Nothing
            , tacRvalue1 = Nothing
            , tacRvalue2 = Nothing
            }]
        mapM_ genBlock allFunctions
        where

            getFunctions :: Dictionary -> [(String, CodeBlock)]
            getFunctions = map mapDictionaryToTuple . findAllFunctionsAndProcedures

            mapDictionaryToTuple :: DictionaryEntry -> (String, CodeBlock)
            mapDictionaryToTuple entry = (name entry, getCodeBlock entry)

            genBlock :: (String, CodeBlock) -> CodeGenMonad ()
            genBlock (funName, codeblock@(CodeBlock _ maxOffset)) = do
                setTempOffset $ alignedOffset maxOffset
                genLabel $ Label funName
                t <- newtemp
                putArrayOffsetVar t
                genIdAssignment (Id t) $ Constant ("TO_REPLACE", BigIntTAC)
                genCode codeblock
                offset <- getTempOffset
                addTemp t offset

instance GenerateCode CodeBlock where
    genCode (CodeBlock instrs _) = mapM_ genCode instrs

instance GenerateCode Instruction where
    genCode instruction = do
        next <- newLabel
        genCodeForInstruction instruction next

genCodeForInstruction :: Instruction -> OperandType -> CodeGenMonad ()

genCodeForInstruction (InstInitArray arrayId@(G.Id _ s)) _ = do
    entry <- findSymEntryById arrayId <$> ask
    let e@(ST.Simple t) = extractTypeFromExtra entry
    allocatedSize <- buildFromType (t, e)
    offsetVar <- Id <$> getArrayOffsetVar
    genIdAssignment (Id $ TACVariable entry (getOffset entry)) offsetVar
    newOffset <- newtemp
    gen [ThreeAddressCode
                { tacOperand = Add
                , tacLvalue = Just $ Id newOffset
                , tacRvalue1 = Just offsetVar
                , tacRvalue2 = Just allocatedSize
                }]
    putArrayOffsetVar newOffset
    where
        getTypeFromString :: String -> CodeGenMonad ST.Extra
        getTypeFromString t = extractTypeFromExtra . findSymEntryByName t <$> ask

        buildForArrayLike :: String -> Expr -> (String, ST.Extra) -> CodeGenMonad OperandType
        buildForArrayLike typeName sizeExpr t = do
            operand <- genCode' sizeExpr
            temp <- Id <$> newtemp
            genIdAssignment temp operand
            putArrayEntrySize typeName temp
            allocatedSize <- buildFromType t
            finalAllocatedSize <- Id <$> newtemp

            -- ?INFO(Andres): Solves issue of `mul $reg 1 7`
            midId <- Id <$> newtemp
            gen [ThreeAddressCode
                    { tacOperand = Assign
                    , tacLvalue = Just midId
                    , tacRvalue1 = Just allocatedSize
                    , tacRvalue2 = Nothing
                    }]

            gen [ThreeAddressCode
                { tacOperand = Mult
                , tacLvalue = Just finalAllocatedSize
                , tacRvalue1 = Just midId
                , tacRvalue2 = Just operand
                }]
            return finalAllocatedSize

        buildFromType :: (String, ST.Extra) -> CodeGenMonad OperandType
        buildFromType (_, ST.Simple t)
            | t `elem` ST.definedTypes = do
                t' <- findSymEntryByName t <$> ask
                let (ST.Width w) = findWidth t'
                return $ Constant (show w, BigIntTAC)
            | otherwise = do
                t' <- getTypeFromString t
                buildFromType (t, t')

        buildFromType (typeName, ST.CompoundRec _ expr t'@(ST.Simple t)) =
            buildForArrayLike typeName expr (t, t')

        buildFromType (typeName, ST.Compound t expr) =
            buildForArrayLike typeName expr (ST.sign, ST.Simple ST.sign)

        buildFromType (typeName, ST.Fields _ _) = do
            entry <- findSymEntryByName typeName <$> ask
            let (ST.Width w) = findWidth entry
            let w' = alignedOffset w
            return $ Constant (show w', BigIntTAC)
        buildFromType o = error $ "Error processing " ++ show o

-- Utility instructions
genCodeForInstruction InstReturn _ =
    gen [ThreeAddressCode
            { tacOperand = Return
            , tacLvalue = Nothing
            , tacRvalue1 = Nothing
            , tacRvalue2 = Nothing
            }]

genCodeForInstruction (InstReturnWith expr) _ = do
    operand <- genCode' expr
    midId <- Id <$> newtemp
    -- ?INFO(Andres): Solves issue of `return 98`
    gen [ThreeAddressCode
            { tacOperand = Assign
            , tacLvalue = Just midId
            , tacRvalue1 = Just operand
            , tacRvalue2 = Nothing
            },
            ThreeAddressCode
            { tacOperand = Return
            , tacLvalue = Nothing
            , tacRvalue1 = Just midId
            , tacRvalue2 = Nothing
            }]

{-
For functions/procedures calls we only generate the code for each parameter and call the function
as it was a label
-}
genCodeForInstruction (InstCall fId params) _ = do
    paramsLength <- genParams params
    funEntry <- findSymEntryById fId <$> ask
    gen [ThreeAddressCode
            { tacOperand = Call
            , tacLvalue = Nothing
            , tacRvalue1 = Just $ Label $ name funEntry
            , tacRvalue2 = Just $ Constant (show paramsLength, SmallIntTAC)
            }]

-- Assignments, currently supported for id assignments, structs & unions
genCodeForInstruction (InstAsig lvalue rvalue) next =
    if supportedLvalue lvalue then do
        if expType rvalue == StructLitT then assignStructLiteral lvalue rvalue
        else if isArrayLikeExpr rvalue then
            handleArrayLiteralAssignment 0 lvalue rvalue
        else if isIndexExpr lvalue then
            handleIndexAssignment lvalue rvalue
        else if expType rvalue /= TrileanT || (isIdExpr lvalue && isFunCallExpr rvalue) then
            handleNonBooleanAssignment lvalue rvalue
        else handleSimpleBooleanAssignment lvalue rvalue

        -- The following code takes care of the isActive attribute for unions on property assignments
        when (isUnionBExpr $ expAst lvalue) $ do
            let Expr { expAst = (Access unionExpr propId) } = lvalue
            markActiveAttrForUnion unionExpr propId

    else error $ "Lvalue currently not supported for assignments: " ++ show lvalue
    where
        handleNonBooleanAssignment :: Expr -> Expr -> CodeGenMonad ()
        handleNonBooleanAssignment lvalue rvalue = do
            let Expr { expType = lvalT, expAst = lvalAst } = lvalue
            operand <- genCodeForExpr lvalT lvalAst
            rValueAddress <- genCode' rvalue
            genIdAssignment operand rValueAddress
        handleIndexAssignment :: Expr -> Expr -> CodeGenMonad ()
        handleIndexAssignment lvaule rvalue = do
            let IndexAccess array index = expAst lvalue
            (arrayRefOperand, _, base) <- genIndexAccess array index
            operand <- genCode' rvalue
            genSetAssignment base arrayRefOperand operand

        handleSimpleBooleanAssignment :: Expr -> Expr -> CodeGenMonad ()
        handleSimpleBooleanAssignment lvalue rvalue = do
            operand <- genCode' lvalue
            trueLabel <- newLabel
            falseLabel <- newLabel
            genCodeForBooleanExpr (expAst rvalue) trueLabel falseLabel
            genLabel trueLabel
            genIdAssignment operand $ Constant ("true", TrileanTAC)
            genGoTo next
            genLabel falseLabel
            genIdAssignment operand $ Constant ("false", TrileanTAC)
            genLabel next
        supportedLvalue :: Expr -> Bool
        supportedLvalue Expr {expAst = IdExpr _ }       = True
        supportedLvalue Expr {expAst = Access _ _}      = True
        supportedLvalue Expr {expAst = IndexAccess _ _} = True
        supportedLvalue _                               = False

        isIdExpr :: Expr -> Bool
        isIdExpr Expr { expAst = IdExpr _ } = True
        isIdExpr _                          = False

        isIndexExpr :: Expr -> Bool
        isIndexExpr Expr { expAst = IndexAccess _ _ } = True
        isIndexExpr _                                 = False

        isFunCallExpr :: Expr -> Bool
        isFunCallExpr Expr { expAst = EvalFunc _ _ } = True
        isFunCallExpr _                              = False

        isUnionBExpr :: BaseExpr -> Bool
        isUnionBExpr (Access Expr { expType = UnionT _ _ } _) = True
        isUnionBExpr _                                        = False

        isArrayLikeExpr :: Expr -> Bool
        isArrayLikeExpr e = case expAst e of
            ArrayLit _  -> True
            StringLit _ -> True
            _           -> False

        isUnionT :: Type -> Bool
        isUnionT (UnionT _ _) = True
        isUnionT _            = False

        markActiveAttrForUnion :: Expr -> G.Id -> CodeGenMonad ()
        markActiveAttrForUnion unionExpr propId = do
            propertySymEntry <- findSymEntryById propId <$> ask
            let argPos = getUnionAttrId propertySymEntry
            unionExprOp <- genCode' unionExpr
            genIdAssignment unionExprOp $ Constant (show argPos, BigIntTAC)

        assignStructLiteral :: Expr -> Expr -> CodeGenMonad ()
        assignStructLiteral lvalue Expr { expAst = StructLit propAssignments } = do
            mapM_ (assignPropertyValue lvalue) propAssignments

            -- The following code takes care of the isActive attribute for unions on literal assignments
            when (isUnionT $ expType lvalue) $
                let [(propId, _)] = propAssignments in markActiveAttrForUnion lvalue propId

        assignPropertyValue :: Expr -> (G.Id, Expr) -> CodeGenMonad ()
        assignPropertyValue lvalue (propId, e@Expr { expType = eT }) = do
            lOperand <- genCodeForExpr eT (Access lvalue propId)
            rOperand <- genCode' e
            genIdAssignment lOperand rOperand

        handleArrayLiteralAssignment :: Int -> Expr -> Expr -> CodeGenMonad ()
        handleArrayLiteralAssignment index lvalue rvalue =
            case getNextValueFromArrayLike rvalue of
                Nothing -> return ()
                Just (nextValue, nextRValue) -> do
                    let indexExpr = convertToIndexExpr index lvalue
                    next <- newLabel
                    genCodeForInstruction (InstAsig indexExpr nextValue) next
                    handleArrayLiteralAssignment (index + 1) lvalue nextRValue

        convertToIndexExpr :: Int -> Expr -> Expr
        convertToIndexExpr index arrayLikeExpr = Expr {
            expAst = IndexAccess arrayLikeExpr Expr {expAst=IntLit index, expType = BigIntT},
            expType = fromJust $ getTypeFromContainer $ expType arrayLikeExpr
            }

        -- Get next value from an array like. It returns a tuple (Expr, Expr) where
        -- the first element is the next value to index and the second the next
        -- value to pass to this function on next call
        getNextValueFromArrayLike :: Expr -> Maybe (Expr, Expr)
        getNextValueFromArrayLike expr = case expAst expr of
            StringLit "" -> Nothing
            StringLit (c:chs) -> Just (Expr {expAst = CharLit c, expType = CharT}, Expr {expAst = StringLit chs, expType = StringT})
            ArrayLit [] -> Nothing
            ArrayLit (c:chs) -> Just (c, Expr {expAst = ArrayLit chs, expType = expType expr})
            a -> Nothing

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
Structured looping statement
Code generation is similar to that of a bounded loop statement
that iterates on a structure using its indexes,
and additional instructions are added underneath the code block
in order to successfully update the value of the iteration variable
after every iteration.
-}
genCodeForInstruction (InstForEach id@(G.Id tk si) collection codeblock) next = do
    (begin, trueLabel, falseLabel) <- setUpIteration next

    -- Getting the size, which will serve as the iteration bound
    sizeTemp <- genCodeForSize collection
    incrementTemp <- genCodeForExpr BigIntT (IntLit 1)

    -- Setting up the iteration variable
    iterStart <- genCodeForExpr BigIntT (IntLit 0)
    iterTemp <- Id <$> newtemp
    genIdAssignment iterTemp iterStart

    -- Loop header
    genLabel begin
    -- The comparison is fall-through optimized
    genBooleanComparison iterTemp sizeTemp falseLabel trueLabel G.Gte
    genLabel trueLabel

    -- Creating a fictitious access for the array
    let elementType = fromJust . getTypeFromContainer . expType $ collection
    let indexExpr = Expr { expAst = IdExpr (G.Id (tk {cleanedString=show iterTemp}) si), expType = elementType }
    let accessBaseExpr = IndexAccess collection indexExpr

    -- Preparating access code
    (arrayRefOperand, _, base) <- genIndexAccess' collection iterTemp

    -- Assigning access to the id
    idEntry <- findSymEntryById id <$> ask
    let idOp = Id $ TACVariable idEntry (getOffset idEntry)
    gen [ThreeAddressCode
        { tacOperand = Get
        , tacLvalue = Just idOp
        , tacRvalue1 = Just base
        , tacRvalue2 = Just arrayRefOperand
        }]

    -- Codeblock generation
    genCode codeblock

    -- Increment before next iteration
    incOperand <- genOp2Code Add iterTemp incrementTemp
    genIdAssignment iterTemp incOperand

    -- Iterate!
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

    -- Copying original value of step & bound
    stepOperand <- copyOriginalValue step
    boundOperand <- copyOriginalValue bound

    -- Loop generation
    genLabel begin
    idOperand <- genCodeForExpr BigIntT (IdExpr id)
    genBooleanComparison idOperand boundOperand falseLabel trueLabel G.Gte
    genLabel trueLabel
    genCode codeblock

    -- Increment added after the codeblock
    incOperand <- genOp2Code Add idOperand stepOperand
    genIdAssignment idOperand incOperand

    genGoTo begin
    genLabel next
    where
        copyOriginalValue :: Expr -> CodeGenMonad OperandType
        copyOriginalValue expr = do
            originalValue <- genCode' expr
            copyOp <- Id <$> newtemp
            genIdAssignment copyOp originalValue
            return copyOp

{-
Read statement

Given that we can only read values for scalar types + strings, we simply
transform the Read statement into the TAC instruction.

?NOTE: Given that we include trilean support, we might have to modify this
? in order to read trileans as strings and then parse the received value.
-}
genCodeForInstruction (InstRead expr) next = do
    readOperand <- genCode' expr
    gen [ThreeAddressCode
            { tacOperand = Read
            , tacLvalue = Nothing
            , tacRvalue1 = Just readOperand
            , tacRvalue2 = Nothing
            }]
    genLabel next

{-
Print statement

Given that we can only print values for scalar types + strings, we simply
transform the print statement into the TAC instruction.
-}
genCodeForInstruction (InstPrint expr) next = do
    printOperand <- genCode' expr
    gen [ThreeAddressCode
            { tacOperand = Print
            , tacLvalue = Nothing
            , tacRvalue1 = Just printOperand
            , tacRvalue2 = Nothing
            }]
    genLabel next

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
