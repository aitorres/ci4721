module ProgramStructureSpec where

import           FireLink.FrontEnd.Lexer
import           FireLink.FrontEnd.Tokens
import           Test.Hspec
import           Utils                    (getAbstractToken)

spec :: Spec
spec = describe "Lexer" $ do
  it "accepts `--` as a valid token for comments" $ do
    let x = "--"
    let ([], toks) = scanTokens x
    toks `shouldSatisfy` null

  it "accepts `hello ashen one` as a valid token" $ do
    let x = "hello ashen one"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkProgramBegin

  it "accepts `farewell ashen one` as a valid token" $ do
    let x = "farewell ashen one"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkProgramEnd

  it "accepts `in your inventory` as a valid token" $ do
    let x = "in your inventory"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkDeclarationEnd

  it "accepts `traveling somewhere` as a valid token" $ do
    let x = "traveling somewhere"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkInstructionBegin

  it "accepts `you died` as a valid token" $ do
    let x = "you died"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkInstructionEnd

  it "accepts `\\` as a valid token" $ do
    let x = "\\"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkSeq

  it "accepts `with orange soapstone say` as a valid token" $ do
    let x = "with orange soapstone say"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkPrint

  it "accepts `transpose into` as a valid token" $ do
    let x = "transpose into"
    let ([], toks) = scanTokens x
    let atok = getAbstractToken $ head toks
    atok `shouldBe` TkRead
