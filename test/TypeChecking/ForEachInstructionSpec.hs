module ForEachInstructionSpec where

import           Test.Hspec
import qualified TestUtils  as U

baseProgram :: String -> String -> String
baseProgram t1 t2 = "hello ashen one\n\

\traveling somewhere \n\
\with\n\
\   var i of type " ++ t1 ++ ",\n\
\   var is of type " ++ t2 ++ "\n\
\in your inventory\n\
\repairing i with titanite from is\n\
\traveling somewhere\n\
    \with orange soapstone say i\n\
\you died\n\
\weaponry repaired\n\
\you died\n\

\farewell ashen one"

spec :: Spec
spec =
    describe "`for-each` statements" $ do
        it "should accept a program with container as an structure and an iterator of the wrapped type" $
            U.shouldNotError $ baseProgram "sign" "armor of type sign"

        it "should reject a program with non-container as an structure and an iterator of the wrapped type" $
            baseProgram "sign" "sign" `U.shouldErrorOn` ("is", 7, 32)

        it "should reject a program with an iterator variable of different type of the container" $
            baseProgram "humanity" "armor of type sign" `U.shouldErrorOn` ("i", 7, 11)

        it "should reject a program that modifies the iterable container variable" $
            "hello ashen one\n\

            \traveling somewhere \n\
            \with\n\
            \   var i of type sign,\n\
            \   var is of type armor of type sign\n\
            \in your inventory\n\
            \repairing i with titanite from is\n\
            \traveling somewhere\n\
            \   is <<= is union {$i$}\n\
            \you died\n\
            \weaponry repaired\n\
            \you died\n\

            \farewell ashen one" `U.shouldErrorOn` ("is", 9, 4)
        it "should reject a program that modifies the iterable container variable though its in a deeper scope" $
            "hello ashen one\n\

            \traveling somewhere \n\
            \with\n\
            \   var i of type sign,\n\
            \   var is of type armor of type sign,\n\
            \   var j of type humanity,\n\
            \   var js of type armor of type humanity\n\
            \in your inventory\n\
            \repairing i with titanite from is\n\
            \traveling somewhere\n\
            \   repairing j with titanite from js\n\
            \   traveling somewhere\n\
            \      is <<= is union {$i$}\n\
            \   you died\n\
            \weaponry repaired\n\
            \you died\n\
            \weaponry repaired\n\
            \you died\n\

            \farewell ashen one" `U.shouldErrorOn` ("is", 13, 7)
