module InstructionsSpec where

import           FireLink.FrontEnd.Grammar
import           FireLink.FrontEnd.Tokens
import           Test.Hspec
import           Utils

buildProgram :: String -> String
buildProgram s = "\
\ hello ashen one \
\   traveling somewhere \
\       " ++ s ++ "\
\   you died \
\ farewell ashen one"

spec :: Spec
spec = describe "Instructions" $ do
    it "accepts program with only 1 instruction" $
        runTestForValidProgram "\
        \ hello ashen one \

        \ traveling somewhere \
        \   go back \
        \ you died \

        \ farewell ashen one" (\(Program (CodeBlock [InstReturn, InstReturn] _)) -> True)
    it "accepts program with only 2+ instruction" $
        runTestForValidProgram "\
        \ hello ashen one \

        \ traveling somewhere \
        \ with \
        \   var patata of type humanity \
        \ in your inventory \

        \   with orange soapstone say @hello world@ \\ \
        \   transpose into patata \
        \ you died \

        \ farewell ashen one" (\(Program (CodeBlock [
            InstAsig _ Expr{expAst=(IntLit 0)},
            InstPrint Expr{expAst=(StringLit "hello world")},
            InstRead Expr{expAst=(IdExpr (Id Token {cleanedString="patata"} _))}, InstReturn] _)) -> True)

    it "accepts assigning as an instruction" $
        runTestForValidProgram (buildProgram "a <<= 1")
        (\(Program (CodeBlock [
            InstAsig Expr{expAst=(IdExpr (Id Token {cleanedString="a"} _))} Expr{expAst=(IntLit 1)}
        , InstReturn] _)) -> True)
