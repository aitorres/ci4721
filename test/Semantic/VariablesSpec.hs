module VariablesSpec where

import qualified FireLink.FrontEnd.SymTable as ST
import           Test.Hspec
import qualified TestUtils                  as U

program :: String -> String
program e =
  "hello ashen one\n\
  \  traveling somewhere\n\
  \  with\n\
  \    var a of type humanity <<= 1,\n\
  \    const b of type humanity <<= 0,\n\
  \    const c of type <3>-chest of type humanity <<= <$1, 2, 3$>,\n\
  \    var d of type <3>-chest of type humanity <<= <$1, 2, 3$>\n\
  \  in your inventory\n\
  \    "++ e ++ "\\\n\
  \    go back\n\
  \  you died\n\
  \farewell ashen one\n"

test :: U.TestFunction String ST.Dictionary
test prog = U.test (program prog)

testVoid :: U.TestFunction String ()
testVoid prog = U.testVoid (program prog)

spec :: Spec
spec = describe "Constants" $ do
  it "rejects constant reassignments" $ program "b <<= 1" `U.shouldErrorOn` ("b", 9, 5)
  it "allows to use constants on expressions" $ U.shouldNotError $ program "with orange soapstone say b"
  it "rejects constant array reassignments" $ program "c<$0$> <<= 1" `U.shouldErrorOn` ("c", 9, 5)
  it "allows variable reassignments" $ U.shouldNotError $ program "a <<= 3"
  it "allows variable arrays reassignments" $ U.shouldNotError $ program "d<$0$> <<= 3"
