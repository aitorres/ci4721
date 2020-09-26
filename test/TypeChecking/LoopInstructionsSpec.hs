module LoopInstructionsSpec where

import           Test.Hspec
import qualified TestUtils  as U

spec :: Spec
spec =
    describe "`for` statements" $ do
        it "should accept a program with integer steps and limit" $
            U.shouldNotError "\
                \hello ashen one\n\
                \traveling somewhere \n\
                \with\n\
                \   var i of type humanity\n\
                \in your inventory\n\

                \upgrading i with 1 souls until level 10\n\
                \traveling somewhere\n\
                    \with orange soapstone say i\n\
                \you died\n\
                \max level reached\n\

                \you died\n\

                \farewell ashen one"

        it "should reject a program with non-integer step" $
            "\
            \hello ashen one\n\
            \traveling somewhere \n\
            \with\n\
            \   var i of type humanity\n\
            \in your inventory\n\

            \upgrading i with |a| souls until level 10\n\
            \traveling somewhere\n\
                \with orange soapstone say i\n\
            \you died\n\
            \max level reached\n\

            \you died\n\

            \farewell ashen one" `U.shouldErrorOn` ("|a|", 6, 18)
        it "should reject a program with non-integer step" $
            "\
            \hello ashen one\n\
            \traveling somewhere \n\
            \with\n\
            \   var i of type humanity\n\
            \in your inventory\n\

            \upgrading i with 1 souls until level |t|\n\
            \traveling somewhere\n\
                \with orange soapstone say i\n\
            \you died\n\
            \max level reached\n\

            \you died\n\

            \farewell ashen one" `U.shouldErrorOn` ("|t|", 6, 38)
