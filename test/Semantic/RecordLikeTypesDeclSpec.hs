module RecordLikeTypesDeclSpec where

import qualified FireLink.FrontEnd.SymTable as ST
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
    describe "Record like variable declarations" $ do
        it "allows declare variable of `record` type" $ do
            dict <- test "bezel { y of type humanity, z of type sign }"
                varEntry{ST.entryType = Just "_alias_0"} U.extractSimpleFromExtra
                (\(ST.Simple "_alias_0") -> True)
            U.testEntry dict varEntry
                { ST.name="_alias_0"
                , ST.category=ST.Type
                , ST.entryType = Just "bezel"
                } U.extractFieldsFromExtra (\(ST.Fields ST.Record 2) -> True)
            U.testEntry dict varEntry
                { ST.name="y"
                , ST.category=ST.RecordItem
                , ST.scope=2
                , ST.entryType=Just "humanity"} U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
            U.testEntry dict varEntry
                { ST.name="z"
                , ST.category=ST.RecordItem
                , ST.scope=2
                , ST.entryType=Just "sign"} U.extractSimpleFromExtra (\(ST.Simple "sign") -> True)
        it "allows declare `union` type variables" $ do
            dict <- test "link { y of type humanity, z of type sign }"
                varEntry{ST.entryType = Just "_alias_0"} U.extractSimpleFromExtra
                    (\(ST.Simple "_alias_0") -> True)
            U.testEntry dict varEntry
                { ST.name="_alias_0"
                , ST.category=ST.Type
                , ST.entryType=Just "link"} U.extractFieldsFromExtra (\(ST.Fields ST.Union 2) -> True)
            U.testEntry dict varEntry
                { ST.name="y"
                , ST.category=ST.UnionItem
                , ST.scope=2
                , ST.entryType=Just "humanity"} U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
            U.testEntry dict varEntry
                { ST.name="z"
                , ST.category=ST.UnionItem
                , ST.scope=2
                , ST.entryType=Just "sign"} U.extractSimpleFromExtra (\(ST.Simple "sign") -> True)
        it "allows to declare record and simple var (in that order)" $ do
            let p = "hello ashen one \

            \ traveling somewhere \

            \ with \
            \ var y of type bezel { z of type sign, a of type bonfire }, \
            \ var b of type humanity \
            \ in your inventory \

            \ go back \
            \ you died \

            \ farewell ashen one"
            (_, ST.SymTable {ST.stDict=dict}, _) <- U.extractSymTable p
            U.testEntry dict varEntry
                { ST.name = "y", ST.entryType = Just "_alias_0" }
                U.extractSimpleFromExtra (\(ST.Simple "_alias_0") -> True)
            U.testEntry dict varEntry
                { ST.name = "b", ST.entryType = Just "humanity" }
                U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
        it "allows to declare record with already used name properties" $ do
            let p = "hello ashen one\n \

            \ traveling somewhere\n \

            \ with\n \
            \   var x of type bezel { \
            \         x of type bezel { \
            \           x of type humanity \
            \         } \
            \       } \
            \ in your inventory\n \

            \ go back \

            \ you died \

            \ farewell ashen one"
            (_, ST.SymTable {ST.stDict=dict}, errors) <- U.extractSymTable p
            errors `shouldSatisfy` null
            U.testEntry dict varEntry
                { ST.entryType = Just "_alias_1", ST.scope = 1, ST.category = ST.Variable }
                U.extractSimpleFromExtra (\(ST.Simple "_alias_1") -> True)
            U.testEntry dict varEntry
                { ST.entryType = Just "bezel", ST.name = "_alias_1", ST.category = ST.Type }
                U.extractFieldsFromExtra (\(ST.Fields ST.Record 2) -> True)
            U.testEntry dict varEntry
                { ST.entryType = Just "_alias_0" , ST.scope = 2, ST.category = ST.RecordItem }
                U.extractSimpleFromExtra (\(ST.Simple "_alias_0") -> True)
            U.testEntry dict varEntry
                { ST.entryType = Just "bezel", ST.name = "_alias_0", ST.category = ST.Type
                , ST.scope = 2 }
                U.extractFieldsFromExtra (\(ST.Fields ST.Record 3) -> True)
            U.testEntry dict varEntry
                { ST.entryType = Just "humanity", ST.scope = 3, ST.category = ST.RecordItem }
                U.extractSimpleFromExtra (\(ST.Simple "humanity") -> True)
    describe "Record like variable accessing" $ do
        it "allows to access ST.records properties" $
            U.shouldNotError "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type bezel {\n\
            \       y of type humanity\n\
            \   }\n\
            \in your inventory\n\

            \   with orange soapstone say x ~> y\n\

            \you died\n\

            \farewell ashen one"
        it "allows to access ST.records properties on deeper levels" $
            U.shouldNotError "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type bezel {\n\
            \       y of type bezel {\n\
            \           z of type humanity\n\
            \       }\n\
            \   }\n\
            \in your inventory\n\

            \   with orange soapstone say x ~> y ~> z\n\

            \you died\n\

            \farewell ashen one"
        it "allows to access ST.records properties with the same name" $
            U.shouldNotError "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type bezel {\n\
            \       x of type humanity\n\
            \   }\n\
            \in your inventory\n\

            \   with orange soapstone say x ~> x\n\

            \you died\n\

            \farewell ashen one"
        it "rejects to access ST.records properties that doesn't exist" $
            "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type bezel {\n\
            \       y of type humanity\n\
            \   }\n\
            \in your inventory\n\

            \   with orange soapstone say x ~> z\n\

            \you died\n\

            \farewell ashen one" `U.shouldErrorOn` ("~>", 8, 32)
        it "allows to access ST.record properties of arrays of records" $
            U.shouldNotError "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type <1>-chest of type bezel {\n\
            \       y of type bezel {\n\
            \           z of type humanity\n\
            \       }\n\
            \   }\n\
            \in your inventory\n\

            \   with orange soapstone say x <$ 0 $> ~> y ~> z\n\

            \you died\n\

            \farewell ashen one"
        it "rejects to access ST.record properties of arrays of any type that is not record" $
            "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type <1>-chest of type humanity\n\
            \in your inventory\n\

            \   with orange soapstone say x<$0$> ~> z\n\

            \you died\n\

            \farewell ashen one" `U.shouldErrorOn` ("~>", 6, 37)
        it "accepts valid x<$0$> ~> y ~> z<$0$> ~> a" $
            U.shouldNotError "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type <1>-chest of type bezel {\n\
            \       y of type bezel {\n\
            \           z of type <1>-chest of type bezel {\n\
            \               a of type humanity\n\
            \           }\n\
            \       }\n\
            \   }\n\
            \in your inventory\n\

            \   with orange soapstone say x<$0$> ~> y ~> z<$0$> ~> a\n\

            \you died\n\

            \farewell ashen one"
        it "rejects invalid x<$0$> ~> y ~> z<$0$> ~> b" $
            "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type <1>-chest of type bezel {\n\
            \       y of type bezel {\n\
            \           z of type <1>-chest of type bezel {\n\
            \               a of type humanity\n\
            \           }\n\
            \       }\n\
            \   }\n\
            \in your inventory\n\

            \   with orange soapstone say x<$0$> ~> y ~> z<$0$> ~> b\n\

            \you died\n\

            \farewell ashen one" `U.shouldErrorOn` ("~>", 12, 52)
        it "accepts valid assigment of a struct literal for record" $
            U.shouldNotError "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type bezel { a of type humanity, b of type hollow } \n\
            \in your inventory\n\

            \   x <<= { a <<= 1, b <<= 5.6 } \n\

            \you died\n\

            \farewell ashen one"
        it "accepts valid assigment of a struct literal for union" $
            U.shouldNotError "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type link { a of type humanity, b of type hollow } \n\
            \in your inventory\n\

            \   x <<= { a <<= 1 } \n\

            \you died\n\

            \farewell ashen one"
        it "accepts valid initialization on declaration of a struct literal for record" $
            U.shouldNotError "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type bezel { a of type humanity, b of type hollow } <<= { a <<= 1, b <<= 5.4 } \n\
            \in your inventory\n\

            \   go back \n\

            \you died\n\

            \farewell ashen one"
        it "accepts valid initialization on declaration of a struct literal for union" $
            U.shouldNotError "hello ashen one\n\

            \traveling somewhere\n\

            \with\n\
            \   var x of type link { a of type humanity, b of type hollow } <<= { b <<= 5.4 } \n\
            \in your inventory\n\

            \   go back \n\

            \you died\n\

            \farewell ashen one"
