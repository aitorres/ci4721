module SwitchInstructionSpec where

import           Test.Hspec
import qualified TestUtils  as U

spec :: Spec
spec =
    describe "`switch` statements" $ do
        it "should accept a program with validly typed guards" $
            U.shouldNotError "\
                \hello ashen one\n\

                \traveling somewhere \n\
                \with var n of type humanity <<= 5 in your inventory \n\
                \enter dungeon with n:\n\
                \   8:\n\
                \       traveling somewhere\n\
                \           with orange soapstone say @test@\n\
                \       you died\n\
                \   5:\n\
                \       traveling somewhere\n\
                \           with orange soapstone say @test@\n\
                \       you died\n\
                \dungeon exited\n\
                \you died \

                \farewell ashen one"

        it "should reject a program with invalidly typed guards" $
            "\
            \hello ashen one\n\

            \traveling somewhere \n\
            \with var n of type humanity <<= 5 in your inventory \n\
            \enter dungeon with n:\n\
            \   8:\n\
            \       traveling somewhere\n\
            \           with orange soapstone say @test@\n\
            \       you died\n\
            \   |a|:\n\
            \       traveling somewhere\n\
            \           with orange soapstone say @test@\n\
            \       you died\n\
            \dungeon exited\n\
            \you died \

            \farewell ashen one" `U.shouldErrorOn` ("|a|", 9, 4)
