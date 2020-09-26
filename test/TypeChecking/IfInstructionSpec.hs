module IfInstructionSpec where

import           Test.Hspec
import qualified TestUtils  as U

spec :: Spec
spec =
    describe "`if` statements" $ do
        it "should accept a program with boolean guards" $
            U.shouldNotError "\
                \hello ashen one\n\

                \traveling somewhere \n\
                \trust your inventory\n\
                \   lit:\n\
                \       traveling somewhere\n\
                \           with orange soapstone say @test@\n\
                \       you died\n\
                \inventory closed\n\
                \you died \

                \farewell ashen one"

        it "should reject a program with non-boolean guards" $
            "hello ashen one\n\

            \traveling somewhere \n\
            \trust your inventory\n\
            \   1 + 1:\n\
            \       traveling somewhere\n\
            \           with orange soapstone say @test@\n\
            \       you died\n\
            \inventory closed\n\
            \you died \

            \farewell ashen one" `U.shouldErrorOn` ("+", 4, 6)
