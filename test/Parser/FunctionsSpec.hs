module FunctionsSpec where

import Test.Hspec
import Grammar
import Utils

spec :: Spec
spec = describe "Functions declarations" $ do
    let buildProgram c = "\
    \ hello ashen one \
    \   " ++ c ++ " \
    \ traveling somewhere \
    \   with orange saponite say @@ \
    \ you died \
    \ farewell ashen one"

    it "accepts no-args functions" $
        runTestForValidProgram (buildProgram "\
        \ invocation fun \
        \ with skill of type humanity \
        \   traveling somewhere \
        \   with orange saponite say @@ \
        \   you died \
        \ after this return to your world") (\(Program _ [
            Function (Id "fun") [] BigInt _
        ] _) -> True)
    it "accepts 1 val-arg functions" $
        runTestForValidProgram (buildProgram "\
        \ invocation fun \
        \ requesting \
        \   val a of type sign \
        \ with skill of type humanity \
        \   traveling somewhere \
        \   with orange saponite say @@ \
        \   you died \
        \ after this return to your world") (\(Program _ [
            Function (Id "fun") [
                MethodDeclaration Val (Id "a") CharT
            ] BigInt _
        ] _) -> True)
    it "accepts 1 ref-arg functions" $
        runTestForValidProgram (buildProgram "\
        \ invocation fun \
        \ requesting \
        \   ref a of type sign \
        \ with skill of type humanity \
        \   traveling somewhere \
        \   with orange saponite say @@ \
        \   you died \
        \ after this return to your world") (\(Program _ [
            Function (Id "fun") [
                MethodDeclaration Ref (Id "a") CharT
            ] BigInt _
        ] _) -> True)
    it "accepts several args functions" $
        runTestForValidProgram (buildProgram "\
        \ invocation fun \
        \ requesting \
        \   val a of type sign, \
        \   ref b of type bonfire \
        \ with skill of type humanity \
        \   traveling somewhere \
        \   with orange saponite say @@ \
        \   you died \
        \ after this return to your world") (\(Program _ [
            Function (Id "fun") [
                MethodDeclaration Ref (Id "b") BoolT,
                MethodDeclaration Val (Id "a") CharT
            ] BigInt _
        ] _) -> True)
