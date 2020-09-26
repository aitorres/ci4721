module FunctionsSpec where

import qualified FireLink.FrontEnd.Grammar  as G
import qualified FireLink.FrontEnd.SymTable as ST
import           Test.Hspec
import qualified TestUtils                  as U

varEntry :: ST.DictionaryEntry
varEntry = ST.DictionaryEntry
    { ST.scope = 1
    , ST.category = ST.Function
    , ST.entryType = Just "humanity"
    , ST.name = "fun"
    , ST.extra = []
    }

spec :: Spec
spec = do
    describe "Function declarations" $ do
        it "allows to declare functions with no arguments" $ do
            let p = "hello ashen one\n\

            \invocation fun\n\
            \with skill of type humanity\n\
            \traveling somewhere\n\
            \   go back with 1\n\
            \you died\n\
            \after this return to your world\n\


            \ traveling somewhere \
            \ with orange soapstone say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, ST.SymTable {ST.stDict=dict}, _) <- U.extractSymTable p
            U.testEntry dict varEntry U.extractCodeblockFromExtra
                (\(ST.CodeBlock (G.CodeBlock [G.InstReturnWith _] _)) -> True)
            U.testEntry dict varEntry U.extractFieldsFromExtra
                (\(ST.Fields ST.Callable _) -> True)
        it "allows to declare functions with one val argument" $ do
            let p = "hello ashen one\n\

            \invocation fun\n\
            \requesting\n\
            \   val x of type humanity\n\
            \with skill of type humanity\n\
            \traveling somewhere\n\
            \   go back with 1\n\
            \you died\n\
            \after this return to your world\n\


            \ traveling somewhere \
            \ with orange soapstone say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, ST.SymTable {ST.stDict=dict}, _) <- U.extractSymTable p
            U.testEntry dict varEntry U.extractFieldsFromExtra
                (\(ST.Fields ST.Callable 2) -> True)
            U.testEntry dict varEntry
                { ST.scope = 2
                , ST.name = "x"
                , ST.category = ST.ValueParam
                } U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
        it "allows to declare functions with one ref argument" $ do
            let p = "hello ashen one\n\

            \invocation fun\n\
            \requesting\n\
            \   ref x of type humanity\n\
            \with skill of type humanity\n\
            \traveling somewhere\n\
            \   go back with 1\n\
            \you died\n\
            \after this return to your world\n\


            \ traveling somewhere \
            \ with orange soapstone say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, ST.SymTable {ST.stDict=dict}, _) <- U.extractSymTable p
            U.testEntry dict varEntry
                { ST.scope = 2
                , ST.name = "x"
                , ST.category = ST.RefParam
                } U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
        it "allows to declare functions with two or more arguments" $ do
            let p = "hello ashen one\n\

            \invocation fun\n\
            \requesting\n\
            \   ref x of type humanity,\n\
            \   val y of type humanity\n\
            \with skill of type humanity\n\
            \traveling somewhere\n\
            \   go back with 1\n\
            \you died\n\
            \after this return to your world\n\


            \ traveling somewhere \
            \ with orange soapstone say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, ST.SymTable {ST.stDict=dict}, _) <- U.extractSymTable p
            U.testEntry dict varEntry
                { ST.scope = 2
                , ST.name = "x"
                , ST.category = ST.RefParam
                } U.extractArgPositionFromExtra (\(ST.ArgPosition 0)-> True)
            U.testEntry dict varEntry
                { ST.scope = 2
                , ST.name = "y"
                , ST.category = ST.ValueParam
                } U.extractArgPositionFromExtra (\(ST.ArgPosition 1) -> True)
        it "rejects to declare functions with repeated arguments" $
            "hello ashen one\n\

            \invocation fun\n\
            \requesting\n\
            \   ref x of type humanity,\n\
            \   val x of type sign\n\
            \with skill of type humanity\n\
            \traveling somewhere\n\
            \   go back with 1\n\
            \you died\n\
            \after this return to your world\n\


            \ traveling somewhere \
            \ with orange soapstone say @hello world@ \
            \ you died \
            \ farewell ashen one" `U.shouldErrorOn` ("x", 5, 8)
        it "rejects to declare variables on the first scope of a function whose names are on the arg list" $
            "hello ashen one\n\

            \invocation fun\n\
            \requesting\n\
            \   ref x of type humanity\n\
            \with skill of type humanity\n\

            \traveling somewhere\n\
            \with\n\
            \   var x of type sign\n\
            \in your inventory\n\
            \   go back with 1\n\
            \you died\n\

            \after this return to your world\n\


            \ traveling somewhere \
            \ with orange soapstone say @hello world@ \
            \ you died \
            \ farewell ashen one" `U.shouldErrorOn` ("x", 8, 8)
        it "rejects to declare variables on the second (2<=) scope of a function whose names are on the arg list" $
            "hello ashen one\n\

            \invocation fun\n\
            \requesting\n\
            \   ref x of type humanity\n\
            \with skill of type humanity\n\

            \traveling somewhere\n\

            \   while the lit covenant is active:\n\

            \   traveling somewhere\n\
            \   with\n\
            \       var x of type sign\n\
            \   in your inventory\n\
            \       go back with 1\n\
            \   you died\n\

            \   covenant left\n\
            \you died\n\

            \after this return to your world\n\


            \ traveling somewhere \
            \ with orange soapstone say @hello world@ \
            \ you died \
            \ farewell ashen one" `U.shouldErrorOn` ("x", 10, 12)
        it "allows to declare more than 1 function" $ do
            let p = "hello ashen one\n\

            \invocation fun1\n\
            \with skill of type humanity\n\

            \traveling somewhere\n\
            \   go back with 1\n\
            \you died\n\

            \after this return to your world\n\

            \invocation fun2\n\
            \with skill of type sign\n\

            \traveling somewhere\n\
            \   go back with |a|\n\
            \you died\n\

            \after this return to your world\n\


            \ traveling somewhere \
            \ with orange soapstone say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, ST.SymTable {ST.stDict=dict}, errors) <- U.extractSymTable p
            errors `shouldSatisfy` null
            U.testEntry dict varEntry
                { ST.scope = 1
                , ST.name = "fun1"
                , ST.entryType = Just "humanity"
                } U.extractFieldsFromExtra (const True)
            U.testEntry dict varEntry
                { ST.scope = 1
                , ST.name = "fun2"
                , ST.entryType = Just "sign"
                } U.extractFieldsFromExtra (const True)
    describe "Function calls" $ do
        it "allows to call declared functions with no parameters" $
            U.shouldNotError "hello ashen one\n\

            \invocation fun\n\
            \with skill of type humanity\n\

            \traveling somewhere\n\
            \   go back with 123\n\
            \you died\n\

            \after this return to your world\n\


            \ traveling somewhere \
            \ with orange soapstone say summon fun \
            \ you died \
            \ farewell ashen one"
        it "allows to call declared functions with parameters" $
            U.shouldNotError "hello ashen one\n\

            \invocation fun\n\
            \with skill of type humanity\n\

            \traveling somewhere\n\
            \   go back with 123\n\
            \you died\n\

            \after this return to your world\n\


            \ traveling somewhere\n\
            \ with orange soapstone say summon fun\n\
            \ you died \
            \ farewell ashen one"
        it "rejects to call non-declared functions" $
            "hello ashen one\n\

            \invocation fun\n\
            \with skill of type humanity\n\

            \traveling somewhere\n\
            \   go back with 123\n\
            \you died\n\

            \after this return to your world\n\


            \ traveling somewhere\n\
            \   with orange soapstone say summon fun2\n\
            \ you died \
            \ farewell ashen one" `U.shouldErrorOn` ("fun2", 9, 37)
        it "allows recursion" $
            U.shouldNotError "hello ashen one\n\

            \invocation fun\n\
            \with skill of type humanity\n\

            \traveling somewhere\n\
            \   go back with summon fun\n\
            \you died\n\

            \after this return to your world\n\


            \ traveling somewhere\n\
            \   with orange soapstone say summon fun\n\
            \ you died \
            \ farewell ashen one"
        it "allows corecursion" $ do
            let p = "hello ashen one\n\

            \invocation fun\n\
            \with skill of type humanity\n\

            \traveling somewhere\n\
            \   go back with summon fun1\n\
            \you died\n\

            \after this return to your world\n\

            \invocation fun1\n\
            \with skill of type humanity\n\

            \traveling somewhere\n\
            \   go back with summon fun\n\
            \you died\n\

            \after this return to your world\n\


            \ traveling somewhere\n\
            \   with orange soapstone say summon fun\n\
            \ you died \
            \ farewell ashen one"
            errors <- U.extractErrors p
            errors `shouldSatisfy` null
