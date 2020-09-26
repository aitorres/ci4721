module ConditionalStructuresSpec where

import           FireLink.FrontEnd.Grammar
import           FireLink.FrontEnd.Tokens
import           Test.Hspec
import           Utils


spec :: Spec
spec = do
    describe "Classic Selection" $ do
        let buildProgram c = "\
        \ hello ashen one \

        \ traveling somewhere \
        \   trust your inventory \
        \   " ++ c ++ " \
        \   inventory closed \
        \ you died \

        \ farewell ashen one"

        it "rejects empty selection block" $
            runTestForInvalidProgram $ buildProgram ""
        it "rejects selection block with only an else statement" $
            runTestForInvalidProgram $ buildProgram "\
            \ liar!: \
            \   traveling somewhere \
            \       with orange soapstone say @hello world@ \
            \   you died"
        it "accepts a selection block with only one if statment" $
            runTestForValidProgram (buildProgram "\
            \ lit: \
            \   traveling somewhere \
            \       with orange soapstone say @hello world@ \
            \ you died") (\(Program (CodeBlock [InstIf [
                GuardedCase
                    Expr{expAst=TrueLit}
                    (CodeBlock [InstPrint Expr{expAst=(StringLit "hello world")}] _)
                ], InstReturn] _)) -> True)

        it "accepts a selection block with sevearal if/elseif statments and parses them in order" $
            runTestForValidProgram (buildProgram "\
            \ lit: \
            \   traveling somewhere \
            \       with orange soapstone say @hello world@ \
            \   you died \
            \ unlit: \
            \   traveling somewhere \
            \       with orange soapstone say @goodbye@ \
            \   you died") (\(Program (CodeBlock [InstIf [
                GuardedCase
                    Expr{expAst=TrueLit}
                    (CodeBlock [InstPrint Expr{expAst=(StringLit "hello world")}] _),
                GuardedCase Expr{expAst=FalseLit} (CodeBlock [InstPrint Expr{expAst=(StringLit "goodbye")}] _)
                ], InstReturn] _)) -> True)

        it "accepts a selection block with an if and else statment" $
            runTestForValidProgram (buildProgram "\
            \ lit: \
            \   traveling somewhere \
            \       with orange soapstone say @hello world@ \
            \   you died \
            \ liar!: \
            \   traveling somewhere \
            \       with orange soapstone say @goodbye@ \
            \   you died") (\(Program (CodeBlock [InstIf [
                GuardedCase
                    Expr{expAst=TrueLit}
                    (CodeBlock [InstPrint Expr{expAst=(StringLit "hello world")}] _),
                GuardedCase
                    Expr{expAst=TrueLit}
                    (CodeBlock [InstPrint Expr{expAst=(StringLit "goodbye")}] _)
                ], InstReturn] _)) -> True)
        it "rejects a selection block with more than 1 else statment" $
            runTestForInvalidProgram $ buildProgram "\
            \ lit: \
            \   traveling somewhere \
            \       with orange soapstone say @hello world@ \
            \   you died \
            \ liar!: \
            \   traveling somewhere \
            \       with orange soapstone say @hello world@ \
            \   you died \
            \ liar!: \
            \   traveling somewhere \
            \       with orange soapstone say @hello world@ \
            \   you died"
    describe "Switch statements" $ do
        let buildProgram c = "\
        \ hello ashen one \

        \ traveling somewhere \
        \   enter dungeon with a: \
        \   " ++ c ++ " \
        \   dungeon exited \
        \ you died \

        \ farewell ashen one"

        it "rejects an empty switch statement" $
            runTestForInvalidProgram $ buildProgram ""
        it "rejects switch statament with only default" $
            runTestForInvalidProgram $ buildProgram "\
            \ empty dungeon: \
            \   traveling somewhere \
            \       with orange soapstone say @hello@ \
            \   you died"
        it "accepts switch statament with only one case" $
            runTestForValidProgram (buildProgram "\
            \ 1: \
            \   traveling somewhere \
            \       with orange soapstone say @hello@ \
            \   you died") (\(Program (CodeBlock [InstSwitch Expr{expAst=(IdExpr (Id Token {cleanedString="a"} _))} [
                    Case Expr{expAst=(IntLit 1)} (CodeBlock [InstPrint Expr{expAst=(StringLit "hello")}] _)
                ], InstReturn] _)) -> True)

        it "rejects switch statement with more than 1 default" $
            runTestForInvalidProgram $ buildProgram "\
            \ 1: \
            \   traveling somewhere \
            \       with orange soapstone say @hello@ \
            \   you died \
            \ empty dungeon: \
            \   traveling somewhere \
            \       with orange soapstone say @hello@ \
            \   you died \
            \ empty dungeon: \
            \   traveling somewhere \
            \       with orange soapstone say @hello@ \
            \   you died "
        it "accepts swith statement with several non-default cases" $
            runTestForValidProgram (buildProgram "\
            \ 1: \
            \   traveling somewhere \
            \       with orange soapstone say @hello@ \
            \   you died \
            \ 2: \
            \   traveling somewhere \
            \       with orange soapstone say @bye@ \
            \   you died") (\(Program (CodeBlock [InstSwitch Expr{expAst=(IdExpr (Id Token {cleanedString="a"} _))} [
                    Case Expr{expAst=(IntLit 1)} (CodeBlock [InstPrint Expr{expAst=(StringLit "hello")}] _),
                    Case Expr{expAst=(IntLit 2)} (CodeBlock [InstPrint Expr{expAst=(StringLit "bye")}] _)
                ], InstReturn] _)) -> True)

        it "accepts swith statement with several non-default cases and one default" $
            runTestForValidProgram (buildProgram "\
            \ 1: \
            \   traveling somewhere \
            \       with orange soapstone say @hello@ \
            \   you died \
            \ 2: \
            \   traveling somewhere \
            \       with orange soapstone say @bye@ \
            \   you died \
            \ empty dungeon: \
            \   traveling somewhere \
            \       with orange soapstone say @empty@ \
            \   you died") (\(Program (CodeBlock [InstSwitch Expr{expAst=IdExpr (Id Token {cleanedString="a"} _)} [
                    Case Expr{expAst=(IntLit 1)} (CodeBlock [InstPrint Expr{expAst=StringLit "hello"}] _),
                    Case Expr{expAst=(IntLit 2)} (CodeBlock [InstPrint Expr{expAst=StringLit "bye"}] _),
                    DefaultCase (CodeBlock [InstPrint Expr{expAst=(StringLit "empty")}] _)
                ], InstReturn] _)) -> True)

