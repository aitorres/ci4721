module ProceduresSpec where

import qualified FireLink.FrontEnd.Grammar  as G
import qualified FireLink.FrontEnd.SymTable as ST
import           Test.Hspec
import qualified TestUtils                  as U

varEntry :: ST.DictionaryEntry
varEntry = ST.DictionaryEntry
    { ST.scope = 1
    , ST.category = ST.Procedure
    , ST.entryType = Just "void"
    , ST.name = "fun"
    , ST.extra = []
    }

spec :: Spec
spec = do
    describe "Procedure declarations" $ do
        it "allows to declare procedures with no arguments" $ do
            let p = "hello ashen one\n\

            \spell fun\n\
            \traveling somewhere\n\
            \   go back\n\
            \you died\n\
            \ashen estus flask consumed\n\


            \ traveling somewhere \
            \ with orange soapstone say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, ST.SymTable {ST.stDict=dict}, _) <- U.extractSymTable p
            U.testEntry dict varEntry U.extractCodeblockFromExtra
                (\(ST.CodeBlock (G.CodeBlock [G.InstReturn] _)) -> True)
            U.testEntry dict varEntry U.extractFieldsFromExtra
                (\(ST.Fields ST.Callable _) -> True)
        it "allows to declare procedures with one val argument" $ do
            let p = "hello ashen one\n\

            \spell fun\n\
            \requesting\n\
            \   val x of type humanity\n\
            \to the estus flask\n\
            \traveling somewhere\n\
            \   go back\n\
            \you died\n\
            \ashen estus flask consumed\n\


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
                , ST.entryType = Just "humanity"
                } U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
        it "allows to declare procedures with one ref argument" $ do
            let p = "hello ashen one\n\

            \spell fun\n\
            \requesting\n\
            \   ref x of type humanity\n\
            \to the estus flask\n\
            \traveling somewhere\n\
            \   go back\n\
            \you died\n\
            \ashen estus flask consumed\n\


            \ traveling somewhere \
            \ with orange soapstone say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, ST.SymTable {ST.stDict=dict}, _) <- U.extractSymTable p
            U.testEntry dict varEntry
                { ST.scope = 2
                , ST.name = "x"
                , ST.category = ST.RefParam
                , ST.entryType = Just "humanity"
                } U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
        it "allows to declare procedures with two or more arguments" $ do
            let p = "hello ashen one\n\

            \spell fun\n\
            \requesting\n\
            \   ref x of type humanity,\n\
            \   val y of type humanity\n\
            \to the estus flask\n\
            \traveling somewhere\n\
            \   go back\n\
            \you died\n\
            \ashen estus flask consumed\n\


            \ traveling somewhere \
            \ with orange soapstone say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, ST.SymTable {ST.stDict=dict}, _) <- U.extractSymTable p
            U.testEntry dict varEntry
                { ST.scope = 2
                , ST.name = "x"
                , ST.category = ST.RefParam
                , ST.entryType = Just "humanity"
                } U.extractArgPositionFromExtra (\(ST.ArgPosition 0)-> True)
            U.testEntry dict varEntry
                { ST.scope = 2
                , ST.name = "y"
                , ST.category = ST.ValueParam
                , ST.entryType = Just "humanity"
                } U.extractArgPositionFromExtra (\(ST.ArgPosition 1) -> True)
        it "rejects to declare procedures with repeated arguments" $
            "hello ashen one\n\

            \spell fun\n\
            \requesting\n\
            \   ref x of type humanity,\n\
            \   val x of type sign\n\
            \to the estus flask\n\
            \traveling somewhere\n\
            \   go back\n\
            \you died\n\
            \ashen estus flask consumed\n\


            \ traveling somewhere \
            \ with orange soapstone say @hello world@ \
            \ you died \
            \ farewell ashen one" `U.shouldErrorOn` ("x", 5, 8)
        it "rejects to declare variables on the first scope of a procedure whose names are on the arg list" $
            "hello ashen one\n\

            \spell fun\n\
            \requesting\n\
            \   ref x of type humanity\n\
            \to the estus flask\n\

            \traveling somewhere\n\
            \with\n\
            \   var x of type sign\n\
            \in your inventory\n\
            \   go back\n\
            \you died\n\

            \ashen estus flask consumed\n\


            \ traveling somewhere \
            \ with orange soapstone say @hello world@ \
            \ you died \
            \ farewell ashen one" `U.shouldErrorOn` ("x", 8, 8)
        it "allows to declare more than 1 procedure" $ do
            let p = "hello ashen one\n\

            \spell fun1\n\

            \traveling somewhere\n\
            \   go back\n\
            \you died\n\

            \ashen estus flask consumed\n\

            \spell fun2\n\

            \traveling somewhere\n\
            \   go back\n\
            \you died\n\

            \ashen estus flask consumed\n\


            \ traveling somewhere \
            \ with orange soapstone say @hello world@ \
            \ you died \
            \ farewell ashen one"
            (_, ST.SymTable {ST.stDict=dict}, errors) <- U.extractSymTable p
            errors `shouldSatisfy` null
            U.testEntry dict varEntry
                { ST.scope = 1
                , ST.name = "fun1"
                } U.extractFieldsFromExtra (const True)
            U.testEntry dict varEntry
                { ST.scope = 1
                , ST.name = "fun2"
                } U.extractFieldsFromExtra (const True)
    describe "Procedure calls" $ do
        it "allows to call declared procedures with no parameters" $
            U.shouldNotError "hello ashen one\n\

            \spell fun\n\

            \traveling somewhere\n\
            \   go back\n\
            \you died\n\

            \ashen estus flask consumed\n\


            \ traveling somewhere \
            \   cast fun \
            \ you died \
            \ farewell ashen one"
        it "allows to call declared procedures with parameters" $
            U.shouldNotError "hello ashen one\n\

            \spell fun\n\
            \requesting\n\
            \   val i of type humanity\n\
            \to the estus flask\n\
            \traveling somewhere\n\
            \   go back\n\
            \you died\n\

            \ashen estus flask consumed\n\


            \ traveling somewhere\n\
            \   cast fun offering 1 to the estus flask \n\
            \ you died \
            \ farewell ashen one"
        it "rejects to call non-declared procedures" $
            "hello ashen one\n\

            \spell fun\n\

            \traveling somewhere\n\
            \   go back\n\
            \you died\n\

            \ashen estus flask consumed\n\


            \ traveling somewhere\n\
            \   cast fun2\n\
            \ you died \
            \ farewell ashen one" `U.shouldErrorOn` ("fun2", 8, 9)
        it "allows recursion" $
            U.shouldNotError "hello ashen one\n\

            \spell fun\n\

            \traveling somewhere\n\
            \   cast fun \\\n\
            \   go back\n\
            \you died\n\

            \ashen estus flask consumed\n\


            \ traveling somewhere\n\
            \   cast fun\n\
            \ you died \
            \ farewell ashen one"
        it "allows corecursion" $ do
            let p = "hello ashen one\n\

            \spell fun\n\

            \traveling somewhere\n\
            \   cast fun1 \\ \n\
            \   go back\n\
            \you died\n\

            \ashen estus flask consumed\n\

            \spell fun1\n\

            \traveling somewhere\n\
            \   cast fun \\ \n\
            \   go back\n\
            \you died\n\

            \ashen estus flask consumed\n\


            \ traveling somewhere\n\
            \   cast fun\n\
            \ you died \
            \ farewell ashen one"
            errors <- U.extractErrors p
            errors `shouldSatisfy` null
