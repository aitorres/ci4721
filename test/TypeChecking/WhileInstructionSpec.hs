module WhileInstructionSpec where

import           Test.Hspec
import qualified TestUtils  as U

spec :: Spec
spec =
    describe "`while` statements" $ do
        it "should accept a program with boolean guards" $
            U.shouldNotError "\
                \hello ashen one\n\

                \traveling somewhere \n\
                \while the lit covenant is active:\n\
                \       traveling somewhere\n\
                \           with orange soapstone say @test@\n\
                \       you died\n\
                \covenant left\n\
                \you died \

                \farewell ashen one"

        it "should reject a program with non-boolean guards" $
            "hello ashen one\n\

            \traveling somewhere \n\
            \while the 1 covenant is active:\n\
            \       traveling somewhere\n\
            \           with orange soapstone say @test@\n\
            \       you died\n\
            \covenant left\n\
            \you died \

            \farewell ashen one" `U.shouldErrorOn` ("1", 3, 11)
