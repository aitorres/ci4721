module FireLink (
    firelink
) where

import           Control.Monad                      (unless, when)
import qualified Control.Monad.RWS                  as RWS
import qualified Data.Graph                         as G
import           Data.List                          (groupBy, intercalate)
import qualified Data.Map                           as Map
import           Data.Maybe                         (fromJust)
import qualified Data.Set                           as Set
import           FireLink.BackEnd.BackEndCompiler
import           FireLink.FrontEnd.Errors
import           FireLink.FrontEnd.FrontEndCompiler
import qualified FireLink.FrontEnd.SymTable         as ST
import           FireLink.FrontEnd.Tokens
import           FireLink.Utils
import           System.Directory                   (doesFileExist)
import           System.Environment                 (getArgs)
import           System.Exit                        (exitFailure, exitSuccess)
import           System.FilePath                    (takeExtension)

import           Data.Map.Internal.Debug (showTree)
import           Data.Semigroup          ((<>))
import           Options.Applicative
import           System.IO               (IOMode (..), hGetContents, hPutStr,
                                          openFile, stderr)
import qualified TACType                 as TAC
import           Text.Printf             (printf)

printErr :: String -> IO ()
printErr = hPutStr stderr

prettyPrintSymTable :: ST.SymTable -> IO ()
prettyPrintSymTable ST.SymTable{ST.stDict=dict} = do
    let dictList = Map.toList dict
    mapM_ printKey dictList

printKey :: (String, [ST.DictionaryEntry]) -> IO ()
printKey (name, keys) = do
    printf "Entries for name \"%s\": \n" name
    mapM_ printKey' keys
    putStrLn ""
    where
        printKey' :: ST.DictionaryEntry -> IO ()
        printKey' st = do
            putStrLn ""
            putStr " - "
            print st

groupTokensByRowNumber :: [Either Error Token] -> [[Either Error Token]]
groupTokensByRowNumber = groupBy (\e e' -> getRow e == getRow e')
    where
        getRow e = case e of
                    Left (Error _ pn)         -> row pn
                    Right Token {position=pn} -> row pn

lengthOfLine :: [Either Error Token] -> Int
lengthOfLine tokens = case last tokens of
    Right tk@Token {position=pn} -> column pn + length (show tk)
    Left (Error s pn)            -> column pn + length s

joinTokens :: Int -> [Either Error Token] -> String
joinTokens _ [] = ""
joinTokens c (e:tks) = case e of
    Right tk@Token {capturedString=s, position=pn} ->
        replicate (column pn - c) ' ' ++ show tk ++ joinTokens (column pn + length s) tks
    Left (Error s pn) ->
        replicate (column pn - c) ' ' ++ s ++ joinTokens (column pn + length s) tks

buildRuler :: Int -> String
buildRuler = flip replicate '~'

formatLexError :: (Error, [Either Error Token]) -> String
formatLexError (Error _ Position{row=r, column=c}, tokens) =
    printf "%s%sYOU DIED!!%s Lexical error at line %s%s%d%s, column %s%s%d%s:\n%s\n"
        red bold nocolor
        red bold r nocolor
        red bold c nocolor
        fs
    where
        tokensByRowNumber = groupTokensByRowNumber tokens
        maxSize = foldl max (-1) $ map lengthOfLine tokensByRowNumber
        firstLine = joinTokens 1 (head tokensByRowNumber) ++ "\n"
        restLines = map (joinTokens 1) $ tail tokensByRowNumber
        errorRuler = bold ++ red ++ buildRuler (c-1) ++ "^" ++ buildRuler (maxSize - c) ++ nocolor ++ "\n"
        fs = firstLine ++ errorRuler ++ intercalate "\n" restLines


printLexErrors :: [(Error, [Either Error Token])] -> IO ()
printLexErrors [] = return ()
printLexErrors (errorPair:xs) = do
    printErr $ formatLexError errorPair ++ "\n"
    printLexErrors xs

groupLexErrorWithTokenContext :: [Error] -> [Token] -> [(Error, [Token])]
groupLexErrorWithTokenContext [] _ = []
groupLexErrorWithTokenContext (err@(Error _ pn):errors) tokens =
    (err, tks) : groupLexErrorWithTokenContext errors tokens
    where
        tail' = dropWhile (\Token {position=pn'} -> row pn /= row pn') tokens
        tks = takeWhile (\Token {position=pn'} -> row pn' - row pn <= 4) tail'

insertLexErrorOnContext :: Error -> [Token] -> [Either Error Token]
insertLexErrorOnContext l [] = [Left l]
insertLexErrorOnContext e@(Error _ pn) (tk@Token {position=pn'} : tks) =
    if (row pn' >= row pn) && (column pn' >= column pn)
    then Left e : Right tk : map Right tks
    else Right tk : insertLexErrorOnContext e tks

joinTokensOnly :: [Token] -> String
joinTokensOnly = joinTokens (-1) . map Right

numberOfRows :: [Token] -> Int
numberOfRows = foldl (\cu Token {position=pn} -> cu `max` row pn) (-1)

maxDigits :: [Token] -> Int
maxDigits = length . show . numberOfRows

printProgram :: Bool -> [Token] -> IO ()
printProgram success tks =
    mapM_ printRowAndLine groupedByRowNumber
    where
        groupedByRowNumber :: [[Token]]
        groupedByRowNumber = groupBy (\Token {position=t1} Token {position=t2} -> row t1 == row t2) tks

        printFun :: String -> IO ()
        printFun = if success then putStr else printErr

        printRowAndLine :: [Token] -> IO ()
        printRowAndLine tks' = do
            let Token {position=pn} = head tks'
            let line = row pn
            let lengthOfRowNumber = length $ show line
            printFun $ show line
            printFun $ replicate (maxDigits tks - lengthOfRowNumber) ' '
            printFun "|"
            printFun $ joinTokensOnly tks'
            printFun "\n"

printSemErrors :: [Error] -> [Token] -> IO ()
printSemErrors [] _ = do
    printErr "Fix your semantic mistakes, ashen one.\n"
    return ()
printSemErrors (semError:semErrors) tokens = do
    let (Error errMessage p) = semError
    let tks = filter (\Token{position=p'} -> row p' == row p) tokens
    let preContext = filter (\Token{position=p'} -> row p' == row p - 1) tokens
    let postContext = filter (\Token{position=p'} -> row p' == row p + 1) tokens
    let nDigits = 2 + maxDigits tks
    RWS.when (not $ null preContext) $ printProgram False preContext
    printProgram False tks
    printErr $ buildRuler (nDigits + column p)
    printErr "^\n"
    printErr $ bold ++ red ++ "YOU DIED!!" ++ nocolor ++ " " ++ errMessage ++ "\n"
    RWS.when (not $ null postContext) $ printProgram False postContext
    printErr "\n\n"
    printSemErrors semErrors tokens

raiseCompilerError :: String -> IO ()
raiseCompilerError msg = do
    printErr $ bold ++ red ++ "YOU DIED!!" ++ nocolor ++ " Compiler error\n"
    printErr $ msg ++ "\n"
    exitFailure

failByNonExistantFile :: String -> IO ()
failByNonExistantFile f =
    raiseCompilerError $ "Your journey cannot be started from " ++ f ++ ", ashen one, as " ++ bold ++ "the file could not be found" ++ nocolor ++ "."

failByFileExtension :: IO ()
failByFileExtension =
    raiseCompilerError $ "The filename MUST have the " ++ bold ++ ".souls" ++ nocolor ++ " extension, ashen one."

failByEmptyArgs :: IO ()
failByEmptyArgs =
    raiseCompilerError $ "Your journey must be started from a " ++ bold ++ red ++ "<file path>" ++ nocolor ++ ", ashen one."

failByEmptyFile :: IO ()
failByEmptyFile =
    raiseCompilerError $ "Your journey must not be started from an " ++ bold ++ red ++ "empty file" ++ nocolor ++ ", ashen one."

failByUnknownFlag :: IO ()
failByUnknownFlag =
    raiseCompilerError $ "Your journey must not be started from an " ++ bold ++ red ++ "unrecognized flag" ++ nocolor ++ ", ashen one."

handleLexError :: [Error] -> [Token] -> IO ()
handleLexError errors tokens = do
    printLexErrors
        $ map (\(err, tks) -> (err, insertLexErrorOnContext err tks))
        $ groupLexErrorWithTokenContext errors tokens
    printErr "Fix your lexical mistakes, ashen one.\n"

handleCompileError :: CompilerError -> [Token] -> IO ()
handleCompileError compileError tokens =
    case ceErrorCategory compileError of
        LexError      -> handleLexError (ceErrors compileError) tokens
        SemanticError -> printSemErrors (ceErrors compileError) tokens

printTacCode :: [TAC] -> IO ()
printTacCode = mapM_ print

printBasicBlocks :: [NumberedBlock] -> IO ()
printBasicBlocks = mapM_ printBlock
    where
        printBlock :: NumberedBlock -> IO ()
        printBlock nb = do
            let (i, cb) = nb
            putStrLn $ bold ++ "Block " ++ show i ++ nocolor
            printTacCode cb

            let d = def $ snd nb
            unless (Set.null d) $ do
                putStrLn $ red ++ "Definitions of block " ++ show i ++ nocolor
                putStrLn $ intercalate "\n" $ map show $ Set.toList d
            let u = use $ snd nb
            unless (Set.null u) $ do
                putStrLn $ red ++ "Variable uses of block " ++ show i ++ nocolor
                putStrLn $ intercalate "\n" $ map show $ Set.toList u


printFlowGraph :: G.Graph -> IO ()
printFlowGraph g = do
    let vs = G.vertices g
    let es = G.edges g
    putStrLn $ bold ++ "Graph (" ++ (show . length) vs ++ " nodes, " ++ (show . length) es ++ " edges)" ++ nocolor
    let groupedEdges = groupBy (\a b -> fst a == fst b) es
    mapM_ printEdges groupedEdges
    where
        printEdges :: [G.Edge] -> IO ()
        printEdges es = do
            let origin = (fst . head) es
            let originName = if origin == entryNode then "ENTRY" else show origin
            let destinies = map snd es
            putStrLn $ bold ++ originName ++ nocolor ++ " -> " ++ printDestinies destinies

        printDestinies :: [Int] -> String
        printDestinies destinies = if head destinies == exitNode then bold ++ "EXIT" ++ nocolor else joinWithCommas destinies

        entryNode :: Int
        entryNode = -1

        exitNode :: Int
        exitNode = length (G.vertices g) - 2

printInterferenceGraph'' :: InterferenceGraph -> RegisterAssignment -> IO ()
printInterferenceGraph'' (vertexMap, graph) registerAssignment = do
    let vs = G.vertices graph
    let es = G.edges graph
    putStrLn $ bold ++ "Graph (" ++ (show . length) vs ++ " variables, " ++ (show . length) es ++ " interferences)" ++ nocolor
    -- putStrLn "vertexMap"
    -- putStrLn $ showTree vertexMap
    -- putStrLn "registerAssignment"
    -- putStrLn $ showTree registerAssignment
    mapM_ printEdges vs
    checkValidity vs
    where
        successors :: G.Vertex -> [G.Vertex]
        successors vertex =
            let graphEdges = G.edges graph
                outgoingEdges = filter ((== vertex) . fst) graphEdges
                successors' = map snd outgoingEdges
            in successors'

        printPair :: (TACSymEntry, Register) -> String
        printPair (var, reg) =
            red ++ "(" ++ nocolor ++
            bold ++ show var ++ nocolor ++
            red ++ ", " ++ nocolor ++ bold ++ show reg ++ nocolor ++
            red ++ ")" ++ nocolor

        printEdges :: G.Vertex -> IO ()
        printEdges v = do
            let origin = vertexMap Map.! v
            let originName = show origin
            let assignedRegister = registerAssignment Map.! origin
            let destinies = successors v
            putStrLn $ printPair (origin, assignedRegister) ++ " -> " ++ printDestinies destinies

        printDestinies :: [G.Vertex] -> String
        printDestinies destinies = intercalate ", " $
            map (\vertex -> let var = vertexMap Map.! vertex in printPair (var, registerAssignment Map.! var)) destinies

        checkValidity :: [G.Vertex] -> IO ()
        checkValidity [] = putStrLn "Thankfully, register assignment is correct!"
        checkValidity (v : vs) = do
            let destinies = successors v
            if v `elem` destinies then do
                putStrLn "Sadly, register assignment is wrong :("
                printEdges v
            else checkValidity vs

compile :: String -> CommandLineArgs -> IO ()
compile program args = do
    compilerResult <- frontEnd program
    case compilerResult of
        Left e -> uncurry handleCompileError e >> exitFailure
        Right (ast, symTable, tokens) -> do
            ((numberedBlocks, flowGraph), interferenceGraph, registerAssignment, finalCode) <- backend ast (ST.stDict symTable)
            if showTargetCode args then putStr $ unlines finalCode
            else do
                when (anyArg args) $ do
                    -- Default behavior: print everything
                    putStrLn "\nFireLink: Printing SymTable"
                    prettyPrintSymTable symTable
                    putStrLn "\nFireLink: Printing recognized program"
                    printProgram True tokens
                    putStrLn "\nFireLink: Printing Intermediate Representation in Three-Address Code"
                    printTacCode $ concatMap snd numberedBlocks
                    putStrLn "\nFireLink: Printing basic blocks (numbered, starting at 0)"
                    printBasicBlocks numberedBlocks
                    putStrLn "\nFireLink: Printing flow graph"
                    printFlowGraph flowGraph
                    putStrLn "FireLink: printing register assignment with interference graph"
                    printInterferenceGraph'' interferenceGraph registerAssignment
                    return ()
                when (showSymTable args && showProgram args) $ prettyPrintSymTable symTable >> printProgram True tokens
                when (showSymTable args) $ prettyPrintSymTable symTable
                when (showProgram args) $ printProgram True tokens
                when (showTac args) $ printTacCode $ concatMap snd numberedBlocks
                when (showBlocks args) $ printBasicBlocks numberedBlocks
                when (showGraph args) $ printFlowGraph flowGraph
                when (showRegisterAssignment args) $ printInterferenceGraph'' interferenceGraph registerAssignment
                exitSuccess

anyArg :: CommandLineArgs -> Bool
anyArg args = all not $ map ($ args) [showSymTable, showProgram, showTac, showBlocks, showGraph, showRegisterAssignment, showTargetCode]

data CommandLineArgs = CommandLineArgs
    { filename :: String
    , showSymTable :: Bool
    , showProgram :: Bool
    , showTac :: Bool
    , showBlocks :: Bool
    , showGraph :: Bool
    , showRegisterAssignment :: Bool
    , showTargetCode :: Bool
    }
    deriving Show

commandLineParser :: Parser CommandLineArgs
commandLineParser =
    CommandLineArgs
        <$> argument str
            ( metavar "FILENAME"
            <> value ""
            )
        <*> switch
            ( long "symtable"
            <> short 's'
            <> help "Show symtable" )
        <*> switch
            ( long "program"
            <> short 'p'
            <> help "Print colorized program" )
        <*> switch
            ( long "tac"
            <> short 't'
            <> help "Print all three-address code" )
        <*> switch
            ( long "blocks"
            <> short 'b'
            <> help "Print all basic blocks" )
        <*> switch
            ( long "graph"
            <> short 'g'
            <> help "Print flow graph" )
        <*> switch
            ( long "register"
            <> short 'r'
            <> help "Print register assignment" )
        <*> switch
            ( long "target-code"
            <> short 'c'
            <> help "Print Target Code" )

compiler :: CommandLineArgs -> IO ()
compiler commandLineArgs = do
    let programFile = filename commandLineArgs
    if null programFile then failByEmptyArgs
    else do
        fileExists <- doesFileExist programFile
        if fileExists then
            if takeExtension programFile /= ".souls" then failByFileExtension
            else do
                handle <- openFile programFile ReadMode
                contents <- hGetContents handle
                if null contents then failByEmptyFile
                else compile contents commandLineArgs
        else failByNonExistantFile programFile

firelink :: IO ()
firelink = compiler =<< execParser opts
    -- commandLineArgs <- execParser opts
    -- if null args then failByEmptyArgs
    -- else do
    --     let programFile = head args
    --     let compileFlag = if length args < 2 then Nothing else Just (args !! 1)
    --     fileExists <- doesFileExist programFile
    --     if fileExists then
    --         if takeExtension programFile /= ".souls" then failByFileExtension
    --         else do
    --             handle <- openFile programFile ReadMode
    --             contents <- hGetContents handle
    --             if null contents then failByEmptyFile
    --             else compile contents compileFlag
    --     else failByNonExistantFile programFile
  where
    opts = info (commandLineParser <**> helper)
      ( fullDesc
     <> progDesc "Compiles a FireLink program to MIPS32"
     <> header "firelink - a compiler for FireLink programming language" )
