module SimpleTypesDeclSpec where

import           FireLink.FrontEnd.Errors
import qualified FireLink.FrontEnd.Grammar  as G
import qualified FireLink.FrontEnd.SymTable as ST
import qualified FireLink.FrontEnd.Tokens   as T
import qualified FireLink.Utils             as UU
import           Test.Hspec
import qualified TestUtils                  as U

program :: String -> String
program e = "hello ashen one\
\ traveling somewhere \
\ with \
\   var x of type " ++ e ++ "\
\ in your inventory \
\ go back \
\ you died \
\ farewell ashen one"

varEntry :: ST.DictionaryEntry
varEntry = ST.DictionaryEntry
    { ST.scope = 1
    , ST.category = ST.Variable
    , ST.entryType = Nothing
    , ST.name = "x"
    , ST.extra = []
    }

test :: U.TestFunction String ST.Dictionary
test prog = U.test (program prog)

testVoid :: U.TestFunction String ()
testVoid prog = U.testVoid (program prog)

spec :: Spec
spec = do
    describe "Simple type variable Declarations" $ do
        it "allows to declare variables of type `humanity`" $
            testVoid "humanity" varEntry{ST.entryType = Just "humanity"}
                U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
        it "allows to declare variables of type `small humanity`" $
            testVoid "small humanity" varEntry{ST.entryType = Just "small humanity"}
                U.extractSimpleFromExtra (\(ST.Simple "small humanity") -> True)
        it "allows to declare variables of type `hollow`" $
            testVoid "hollow" varEntry{ST.entryType = Just "hollow"}
                U.extractSimpleFromExtra (\(ST.Simple "hollow") -> True)
        it "allows to declare variables of type `sign`" $
            testVoid "sign" varEntry{ST.entryType = Just "sign"}
                U.extractSimpleFromExtra (\(ST.Simple "sign") -> True)
        it "allows to declare variables of type `bonfire`" $
            testVoid "bonfire" varEntry{ST.entryType = Just "bonfire"}
                U.extractSimpleFromExtra (\(ST.Simple "bonfire") -> True)
        it "allows to declare variables of type `<n>-miracle`" $ do
            dict <- test "<1>-miracle"  varEntry{ST.entryType = Just "_alias_0"}
                U.extractSimpleFromExtra (\(ST.Simple "_alias_0") -> True)
            U.testEntry dict varEntry
                { ST.name = "_alias_0"
                , ST.category = ST.Type
                , ST.entryType = Just ">-miracle" } U.extractCompoundFromExtra
                (\(ST.Compound ">-miracle" G.Expr{G.expAst=G.IntLit 1}) -> True)
        it "allows to declare variables of recursive type `<n>-chest of type humanity" $ do
            dict <- test "<1>-chest of type humanity"  varEntry{ST.entryType = Just "_alias_0"}
                U.extractSimpleFromExtra (\(ST.Simple "_alias_0") -> True)
            U.testEntry dict varEntry
                { ST.name = "_alias_0"
                , ST.category = ST.Type
                , ST.entryType = Just ">-chest" }
                U.extractCompoundRecFromExtra
                    (\(ST.CompoundRec ">-chest"
                        G.Expr{G.expAst=G.IntLit 1}
                        (ST.Simple "humanity")) -> True)
        it "allows to declare variables of recursive type `<n>-chest of type <m>-chest of type humanity" $ do
            dict <- test "<1>-chest of type <2>-chest of type humanity" varEntry{ST.entryType = Just "_alias_1"}
                U.extractSimpleFromExtra (\(ST.Simple "_alias_1") -> True)
            U.testEntry dict varEntry
                { ST.name = "_alias_1"
                , ST.entryType = Just ">-chest"
                , ST.category = ST.Type
                } U.extractCompoundRecFromExtra (\(ST.CompoundRec
                    ">-chest"
                    G.Expr{G.expAst=G.IntLit 1} (ST.Simple "_alias_0")) -> True)
            U.testEntry dict varEntry
                { ST.name = "_alias_0"
                , ST.entryType = Just ">-chest"
                , ST.category = ST.Type
                } U.extractCompoundRecFromExtra (\(ST.CompoundRec ">-chest"
                        G.Expr{G.expAst=G.IntLit 2}
                        (ST.Simple "humanity")) -> True)
        it "allows to declare variables of recursive type `<n>-chest of type <n>-miracle" $ do
            dict <- test "<1>-chest of type <2>-miracle" varEntry{ST.entryType = Just "_alias_1" }
                U.extractSimpleFromExtra (\(ST.Simple "_alias_1") -> True)
            U.testEntry dict varEntry
                { ST.name = "_alias_1"
                , ST.entryType = Just ">-chest"
                , ST.category = ST.Type }
                U.extractCompoundRecFromExtra (\(ST.CompoundRec ">-chest"
                    G.Expr{G.expAst=G.IntLit 1} (ST.Simple "_alias_0")) -> True)
            U.testEntry dict varEntry
                { ST.name = "_alias_0"
                , ST.entryType = Just ">-miracle"
                , ST.category = ST.Type
                }
                U.extractCompoundFromExtra (\(ST.Compound ">-miracle" G.Expr{G.expAst=G.IntLit 2}) -> True)
        it "allows to declare variables of recursive type `armor of type sign" $ do
            dict <- test "armor of type sign" varEntry{ST.entryType = Just "_alias_0",  ST.scope = 1}
                U.extractSimpleFromExtra (\(ST.Simple "_alias_0") -> True)

            U.testEntry dict varEntry
                { ST.entryType = Just "armor"
                , ST.name = "_alias_0"
                , ST.category = ST.Type
                } U.extractRecursiveFromExtra (\(ST.Recursive "armor" (ST.Simple "sign")) -> True)
        it "allows to declare variables of recursive type `armor of type <n>-chest of type sign" $ do
            dict <- test "armor of type <1>-chest of type sign" varEntry{ST.entryType = Just "_alias_1"}
                U.extractSimpleFromExtra (\(ST.Simple "_alias_1") -> True)
            U.testEntry dict varEntry
                { ST.entryType = Just "armor"
                , ST.name = "_alias_1"
                , ST.category = ST.Type}
                U.extractRecursiveFromExtra (\(ST.Recursive "armor" (ST.Simple "_alias_0")) -> True)
            U.testEntry dict varEntry
                { ST.entryType = Just ">-chest"
                , ST.name = "_alias_0"
                , ST.category = ST.Type }
                U.extractCompoundRecFromExtra (\(ST.CompoundRec ">-chest" G.Expr{G.expAst=G.IntLit 1} (ST.Simple "sign")) -> True)
        it "allows declare variables of recursive type `armor of type armor of type sign" $ do
            dict <- test "armor of type armor of type sign" varEntry{ST.entryType = Just "_alias_1"}
                U.extractSimpleFromExtra (\(ST.Simple "_alias_1") -> True)
            U.testEntry dict varEntry
                { ST.entryType = Just "armor"
                , ST.name = "_alias_1"
                , ST.category = ST.Type
                }
                U.extractRecursiveFromExtra (\(ST.Recursive "armor" (ST.Simple "_alias_0")) -> True)

            U.testEntry dict varEntry
                { ST.entryType = Just "armor"
                , ST.name = "_alias_0"
                , ST.category = ST.Type
                }
                U.extractRecursiveFromExtra (\(ST.Recursive "armor" (ST.Simple "sign")) -> True)

        it "allows to declare 2 or more variables" $ do
            let p = "hello ashen one \

            \ traveling somewhere \

            \ with \
            \ var x of type sign, \
            \ var y of type humanity \
            \ in your inventory \

            \ go back \
            \ you died \

            \ farewell ashen one"
            (_, ST.SymTable {ST.stDict=dict}, _) <- U.extractSymTable p

            U.testEntry dict varEntry{ST.entryType = Just "sign"} U.extractSimpleFromExtra
                (\(ST.Simple "sign") -> True)
            U.testEntry dict varEntry{ST.entryType = Just "humanity", ST.name = "y"}
                U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
    describe "Checks for variable redeclarations and scope chain" $ do
        it "allows to declare the same variable name in a deeper level" $ do
            let p = "hello ashen one \

            \ traveling somewhere \

            \ with \
            \   var x of type humanity \
            \ in your inventory \


            \ trust your inventory \
            \ lit: \
            \   traveling somewhere \
            \   with \
            \       var x of type sign \
            \   in your inventory \

            \   go back \
            \   you died \
            \ inventory closed \
            \ you died \

            \ farewell ashen one"
            (_, ST.SymTable {ST.stDict=dict}, _) <- U.extractSymTable p
            U.testEntry dict varEntry
                { ST.entryType = Just "humanity" }
                U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
            U.testEntry dict varEntry
                { ST.entryType = Just "sign", ST.scope = 2 }
                U.extractSimpleFromExtra (\(ST.Simple "sign") -> True)
        it "rejects to declare the same variable name in the same scope" $ do
            let p = "hello ashen one\n \

            \ traveling somewhere\n \

            \ with\n \
            \   var x of type humanity,\n \
            \   var x of type sign\n \
            \ in your inventory\n \


            \ trust your inventory \
            \ lit: \
            \   traveling somewhere \
            \   with \
            \       var x of type sign \
            \   in your inventory \

            \   go back \
            \   you died \
            \ inventory closed \
            \ you died \

            \ farewell ashen one"
            (_, ST.SymTable {ST.stDict=dict}, errors) <- U.extractSymTable p
            errors `shouldNotSatisfy` null
            let Error _ pn = head errors
            UU.row pn `shouldBe` 5
            UU.column pn `shouldBe` 9
            U.testEntry dict varEntry
                { ST.entryType = Just "humanity" }
                U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
            U.testEntry dict varEntry
                { ST.entryType = Just "sign", ST.scope = 2 }
                U.extractSimpleFromExtra (\(ST.Simple "sign") -> True)
    describe "Checks that variable is declared before it is already used" $ do
        it "Allows to use already declared variables" $
            U.shouldNotError "hello ashen one \

            \ traveling somewhere \

            \ with \
            \   var x of type humanity \
            \ in your inventory \
            \ with orange soapstone say x \
            \ you died \

            \ farewell ashen one"
        it "Allows to use already declared variables that were declared in another scope in the same scope chain" $
            U.shouldNotError "hello ashen one\n\

            \ traveling somewhere\n\

            \ with \
            \   var x of type humanity\n\
            \ in your inventory\n\

            \while the lit covenant is active:\n\
            \traveling somewhere\n\
            \   with orange soapstone say x\n\
            \you died\n\
            \covenant left\n\
            \ you died\n\

            \ farewell ashen one\n"
        it "Allows to use already declared variables on assignments" $
            U.shouldNotError "hello ashen one \

            \ traveling somewhere \

            \ with \
            \   var x of type humanity \
            \ in your inventory \
            \   x <<= 1 \
            \ you died \

            \ farewell ashen one"
        it "Rejects to use not declared variables on assignments" $
            "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type humanity\n\
            \in your inventory \n\
            \   y <<= 1\n\
            \you died\n\

            \farewell ashen one" `U.shouldErrorOn` ("y", 6, 4)
        it "Allows to use already declared variables on IO input" $
            U.shouldNotError "hello ashen one \

            \ traveling somewhere \

            \ with \
            \   var x of type humanity \
            \ in your inventory \
            \   transpose into x \
            \ you died \

            \ farewell ashen one"
        it "Rejects to use not declared variables on IO input" $
            "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type humanity\n\
            \in your inventory \n\
            \   transpose into y\n\
            \you died\n\

            \farewell ashen one" `U.shouldErrorOn` ("y", 6, 19)
        it "Allows to use already declared variables on switch statements" $
            U.shouldNotError "hello ashen one \

            \ traveling somewhere \

            \with \
            \   var x of type humanity \
            \in your inventory \
            \   enter dungeon with x:\
            \      3:\
            \       traveling somewhere\
            \           go back\
            \       you died\
            \   dungeon exited \
            \ you died \

            \ farewell ashen one"
        it "Rejects to use not declared variables on switch statements" $
            "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type humanity\n\
            \in your inventory \n\
            \   enter dungeon with y:\n\
            \      undiscovered:\
            \       traveling somewhere\
            \           go back\
            \       you died\
            \   dungeon exited \
            \you died\n\

            \farewell ashen one" `U.shouldErrorOn` ("y", 6, 23)
        it "Rejects to use not declared variables in any scope" $
            "hello ashen one\n\

            \ traveling somewhere\n\

            \ with\n\
            \   var x of type humanity\n\
            \ in your inventory\n\
            \   with orange soapstone say y\n\
            \ you died \

            \ farewell ashen one" `U.shouldErrorOn` ("y", 6, 30)

    describe "Correctly handles initialization (declaration + assignments)" $ do
        it "Prepends assignment instruction on initialization" $ do
            let p = "hello ashen one\n\

            \ traveling somewhere\n\

            \ with\n\
            \   var x of type small humanity <<= 1\n\
            \ in your inventory\n\
            \   with orange soapstone say @oh yes@\n\
            \ you died\n\

            \ farewell ashen one"
            (programAst, _, errors) <- U.extractSymTable p
            errors `shouldSatisfy` null
            programAst `shouldSatisfy`
                (\(G.Program (
                    G.CodeBlock
                        [ G.InstAsig
                            G.Expr{G.expAst=(G.IdExpr (G.Id T.Token {T.cleanedString="x"} _))}
                            G.Expr{G.expAst=(G.IntLit 1)}
                        , G.InstPrint G.Expr{G.expAst=(G.StringLit "oh yes")}
                        , G.InstReturn
                        ] _)) -> True)

        it "Prepends assignment in correct order" $ do
            let p = "hello ashen one\n\

            \ traveling somewhere\n\

            \ with\n\
            \   var x of type small humanity <<= 1,\n\
            \   var y of type small humanity <<= 22\n\
            \ in your inventory\n\
            \   with orange soapstone say @oh yes@\n\
            \ you died\n\

            \ farewell ashen one"
            (programAst, _, errors) <- U.extractSymTable p
            errors `shouldSatisfy` null
            programAst `shouldSatisfy`
                (\(G.Program (
                    G.CodeBlock
                        [ G.InstAsig
                            G.Expr{G.expAst=(G.IdExpr (G.Id T.Token {T.cleanedString="x"} 1))}
                            G.Expr{G.expAst=(G.IntLit 1)}
                        , G.InstAsig
                            G.Expr{G.expAst=(G.IdExpr (G.Id T.Token {T.cleanedString="y"} 1))}
                            G.Expr{G.expAst=(G.IntLit 22)}
                        , G.InstPrint G.Expr{G.expAst=(G.StringLit "oh yes")}
                        , G.InstReturn
                        ] _)) -> True)
