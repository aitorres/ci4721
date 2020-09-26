module IOInstructionsSpec where

import           Test.Hspec
import qualified TestUtils  as U

baseProgram :: String -> String -> String
baseProgram op t = "hello ashen one\n\

\traveling somewhere \n\
\with\n\
\   var x of type " ++ t ++ "\n\
\in your inventory\n\
\   " ++ op ++ " x\n\
\you died\n\

\farewell ashen one"

baseProgramRead :: String -> String
baseProgramRead = baseProgram "transpose into"

baseProgramPrint :: String -> String
baseProgramPrint = baseProgram "with orange soapstone say"

spec :: Spec
spec =
  describe "`IO`" $ do
    it "should accept a read operation with a valid type" $
      U.shouldNotError $ baseProgramRead "humanity"

    it "should accept a print operation with a valid type" $
      U.shouldNotError $ baseProgramPrint "<10>-miracle"

    it "should reject a read operation with an invalid type" $
      baseProgramRead "bezel { x of type humanity }" `U.shouldErrorOn`  ("<<=", 6, 4)

    it "should reject a print operation with an invalid type" $
      baseProgramRead "<15>-chest of type humanity" `U.shouldErrorOn`  ("<<=", 6, 4)
