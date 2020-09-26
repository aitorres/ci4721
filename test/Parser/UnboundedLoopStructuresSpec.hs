module UnboundedLoopStructuresSpec where

import           FireLink.FrontEnd.Grammar
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
spec = describe "Unbounded looping" $ do
    it "rejects an empty unbounded loop" $
        runTestForInvalidProgram $ buildProgram "\
        \ while the lit covenant is active: \
        \ covenant left"
    it "accepts a well-formed unbounded loop" $
        runTestForValidProgram (buildProgram "\
        \ while the unlit covenant is active: \
            \ traveling somewhere \
            \   with orange soapstone say @@ \
            \ you died \
        \ covenant left")  (\(Program (
            CodeBlock [
                InstWhile Expr{expAst=FalseLit} (CodeBlock _ _)
                , InstReturn] _)) -> True)
