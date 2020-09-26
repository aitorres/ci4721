module ForEachLoopStructuresSpec where

import           FireLink.FrontEnd.Grammar
import           FireLink.FrontEnd.Tokens
import           Test.Hspec
import           Utils

buildProgram :: String -> String
buildProgram c = "\
    \ hello ashen one \

    \ traveling somewhere \
    \   " ++ c ++ " \
    \ you died \
    \ farewell ashen one"

spec :: Spec
spec = describe "Loops over structures" $ do
    it "rejects when id is replaced by an expression" $
        runTestForInvalidProgram $ buildProgram "\
        \ repairing 1 with titanite from b \
        \ traveling somewhere \
        \   with orange soapstone say @@ \
        \ you died \
        \ weaponry repaired"
    it "accepts on well-formed loops" $
        runTestForValidProgram (buildProgram "\
        \ repairing a with titanite from b \
        \ traveling somewhere \
        \   with orange soapstone say @@ \
        \ you died \
        \ weaponry repaired") (\(Program (
            CodeBlock [
                InstForEach (Id Token {cleanedString="a"} _) Expr{expAst=IdExpr (Id Token {cleanedString="b"} _)}
                    (CodeBlock [InstPrint Expr{expAst=StringLit ""}] _), InstReturn
                ] _)) -> True)
