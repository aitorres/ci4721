module OffsetSpec where

import qualified FireLink.FrontEnd.SymTable   as ST
import           Test.Hspec
import qualified TestUtils  as U

testOffset :: String -> [(String, Int)] -> IO ()
testOffset = testProgram . baseProgram
    where
        baseProgram :: String -> String
        baseProgram s = "hello ashen one\n\

                    \ traveling somewhere \
                    \ with \
                    \" ++ s ++ "\
                    \ in your inventory \
                    \ with orange soapstone say @hello world@ \
                    \ you died \
                    \ farewell ashen one"

testProgram :: String -> [(String, Int)] -> IO ()
testProgram program testItems = do
    dictionary <- getDictionary program
    mapM_ (test dictionary) testItems

test :: ST.Dictionary -> (String, Int) -> IO ()
test dictionary (varName, expectedOffset) = do
    let chain = filter (\d -> ST.name d == varName) $ ST.findChain varName dictionary
    let dictEntry = head chain
    let ST.Offset actualOffset = U.extractOffsetFromExtra $ ST.extra dictEntry
    (varName, actualOffset) `shouldBe` (varName, expectedOffset)
getDictionary :: String -> IO ST.Dictionary
getDictionary program = do
    ST.SymTable {ST.stDict = dict} <- U.extractDictionary program
    return dict

spec :: Spec
spec =
    describe "Offset calculation" $ do
        it "calculates offset for the first variable in the list" $
            testOffset "var x of type humanity" [("x", 0)]
        it "calculates offset for second variable in the list when first variable's type width is multiple of `wordsize`" $
            testOffset "var x of type humanity, var y of type humanity" [("x", 0), ("y", 4)]
        it "calculates offset for second variable in the list when first variable's type width is not a multiple of `wordSize`" $
            testOffset "var x of type sign, var y of type humanity" [("x", 0), ("y", 4)]
        it "calculates offset for variables with mixed data types" $
            testOffset "\
            \var x1 of type bezel {\
            \   x2 of type humanity,\
            \   x3 of type sign\
            \},\
            \var x4 of type sign" [("x1", 0), ("x2", 0), ("x3", 4), ("x4", 5)]
        it "calculates offset for links" $
            testOffset "\
            \var x1 of type link {\
            \   x2 of type humanity,\
            \   x3 of type sign\
            \}" [("x1", 0), ("x2", 4), ("x3", 4)]
        it "takes advantage of scopes to achieve a better use of offsets" $
            testProgram "hello ashen one\n\
            \ traveling somewhere \
            \ with \
            \   var x of type humanity\
            \ in your inventory \
            \ while the lit covenant is active:\
            \   traveling somewhere\
            \   with\
            \       var y of type humanity\
            \   in your inventory\
            \       while the lit covenant is active:\
            \         traveling somewhere\
            \         with\
            \             var w of type humanity\
            \         in your inventory\
            \             go back\
            \         you died \
            \       covenant left\
            \   you died \
            \ covenant left \\ \
            \ while the lit covenant is active:\
            \   traveling somewhere\
            \   with\
            \       var z of type humanity\
            \   in your inventory\
            \       go back\
            \   you died \
            \ covenant left\
            \ you died \

            \ farewell ashen one" [("x", 0), ("y", 4), ("z", 4), ("w", 8)]
        it "calculates offset for function/procedure arguments, starting on 0" $
            testProgram "hello ashen one\n\

            \ invocation fun1\
            \ requesting\
            \   val x1 of type bezel { x of type sign, y of type bonfire },\
            \   ref x2 of type bezel { x of type humanity, y of type humanity },\
            \   val x3 of type humanity\
            \ with skill of type humanity\
            \   traveling somewhere \
            \   with\
            \       var y1 of type humanity \
            \   in your inventory \
            \   go back with 1\
            \ you died\
            \ after this return to your world\

            \ spell fun2\
            \ requesting\
            \   val x4 of type bezel { x of type sign, y of type bonfire },\
            \   ref x5 of type bezel { x of type humanity, y of type bonfire },\
            \   val x6 of type humanity\
            \ to the estus flask\
            \   traveling somewhere \
            \   with\
            \       var y2 of type humanity \
            \   in your inventory \

            \   go back\
            \ you died\
            \ ashen estus flask consumed\

            \ traveling somewhere \
            \ with \
            \   var x7 of type humanity,\
            \   var x8 of type sign\
            \ in your inventory \
            \ go back \
            \ you died \

            \ farewell ashen one"
                [ ("x1", 0), ("x2", 4), ("x3", 8), ("y1", 12)
                , ("x4", 0), ("x5", 4), ("x6", 8), ("y2", 12)
                , ("x7", 0), ("x8", 4)]
