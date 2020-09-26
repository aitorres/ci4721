module BoundedLoopStructuresSpec where

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
spec = describe "Bounded loop structures" $ do
    it "rejects bounded loop structures when programmer confuses the id with an expression" $
        runTestForInvalidProgram $ buildProgram "\
        \ upgrading |a| with 2 souls until level 123 \
        \   traveling somewhere \
        \       with orange soapstone say @hello@ \
        \   you died \
        \ max level reached"
    it "accepts well formed bounded iterations" $
        runTestForValidProgram (buildProgram "\
        \ upgrading aa with 2 souls until level 123 \
        \   traveling somewhere \
        \       with orange soapstone say @hello@ \
        \   you died \
        \ max level reached") (\(Program (CodeBlock [
            InstFor (Id Token {cleanedString="aa"} _) Expr{expAst=(IntLit 2)} Expr{expAst=(IntLit 123)} (CodeBlock _ _), InstReturn
        ] _)) -> True)
