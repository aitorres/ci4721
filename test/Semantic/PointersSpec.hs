module PointersSpec where

import qualified FireLink.FrontEnd.SymTable as ST
import           Test.Hspec
import qualified TestUtils                  as U

varEntry :: ST.DictionaryEntry
varEntry = ST.DictionaryEntry
    { ST.scope = 1
    , ST.category = ST.Variable
    , ST.entryType = Just "arrow to"
    , ST.name = "x"
    , ST.extra = []
    }

sampleProgram :: String -> String
sampleProgram instr = "hello ashen one\n\

\traveling somewhere\n\
\with\n\
\   var x of type arrow to sign,\n\
\   var y of type humanity\n\
\in your inventory\n\
\   " ++ instr ++ "\n\
\   with orange soapstone say @hello world@\n\
\you died\n\

\farewell ashen one\n"

spec :: Spec
spec = do
    describe "Pointers declarations" $
        it "allows to declare variables with pointers data types" $ do
            let p = sampleProgram ""
            (_, ST.SymTable {ST.stDict=dict}, _) <- U.extractSymTable p
            U.testEntry dict varEntry{ST.entryType = Just "_alias_0"} U.extractSimpleFromExtra
                (\(ST.Simple "_alias_0") -> True)
            let aliasEntry = varEntry{ST.name="_alias_0", ST.category = ST.Type}
            U.testEntry dict aliasEntry U.extractRecursiveFromExtra
                (\(ST.Recursive "arrow to" (ST.Simple "sign")) -> True)


    describe "Pointers operations" $ do
        it "allows to request memory for its usage" $
            U.shouldNotError $ sampleProgram "aim a x \\"
        it "rejects to request memory of non-declared variables" $
            U.shouldErrorOn (sampleProgram "aim a nonDeclaredVariable \\")
                ("nonDeclaredVariable", 7, 10)
        it "rejects to request memory of a non-pointer variable" $
            U.shouldErrorOn (sampleProgram "aim a y \\") ("y", 7, 10)
        it "allows to free memory for its usage" $
            U.shouldNotError $ sampleProgram "recover a x \\"
        it "rejects to free memory of non-declared variables" $
            U.shouldErrorOn (sampleProgram "recover a nonDeclaredVariable \\")
                ("nonDeclaredVariable", 7, 14)
        it "rejects to free memory of a non-pointer variable" $
            U.shouldErrorOn (sampleProgram "recover a y \\") ("y", 7, 14)
        it "allows to access memory address for its usage" $
            U.shouldNotError $ sampleProgram "throw a x <<= |a| \\"
        it "rejects to access memory address of non-declared variables" $
            U.shouldErrorOn (sampleProgram "throw a nonDeclaredVariable <<= |a| \\")
                ("nonDeclaredVariable", 7, 12)
