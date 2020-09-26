module IterationsSpec where

import           Test.Hspec
import qualified TestUtils  as U

spec :: Spec
spec = describe "Iterations declarations" $ do
  it "rejects iteration variable reassignment" $
    "hello ashen one\n\

    \ traveling somewhere \n\
    \ with var i of type humanity <<= 1 in your inventory \n\
    \ upgrading i with 1 soul until level 10 \n\
    \ traveling somewhere \n\
    \ i <<= 1 + 1 \\ \n\
    \ with orange soapstone say @hello world@ \n\
    \ you died \n\
    \ max level reached \n\
    \ you died \n\
    \ farewell ashen one" `U.shouldErrorOn` ("i", 6, 2)
  it "rejects constant usage as an iteration variable" $
    "hello ashen one\n\

    \ traveling somewhere \n\
    \ with const i of type humanity <<= 1 in your inventory \n\
    \ upgrading i with 1 soul until level 10 \n\
    \ traveling somewhere \n\
    \ with orange soapstone say @hello world@ \n\
    \ you died \n\
    \ max level reached \n\
    \ you died \n\
    \ farewell ashen one" `U.shouldErrorOn` ("i", 4, 12)
