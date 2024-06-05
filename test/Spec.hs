-- test/Spec.hs
{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Interpret (interpret)
import Parser (parse)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

testAwkData :: ByteString
testAwkData = "Beth 4.00 0\n\
\Kathy 4.00 10\n\
\Mark 5.00 20\n\
\Dan 3.75 0\n\
\Mary 5.50 22\n\
\Susie 4.25 18\n"

runSystem :: ByteString -> ByteString -> Either String [ByteString]
runSystem script input = do
  parsed <- parse script
  return $ map (interpret parsed) $ BS.lines input

main :: IO ()
main = do
  testColvarBinop
  testPrint

-- Eventually half of these should be flipped. i.e. $3 == 0 should be 0 == $3
testColvarBinop :: IO ()
testColvarBinop = hspec $ do
  describe "testColvarBinop" $ do
    it "ColvarEqDigit" $ do
      runSystem ("$3 == 0 { print $1 }") testAwkData
        `shouldBe` Right ["Beth", BS.empty, BS.empty, "Dan", BS.empty, BS.empty]
    it "ColvarNeDigit" $ do
      runSystem ("$3 != 0 { print $1 }") testAwkData
        `shouldBe` Right [BS.empty, "Kathy", "Mark", BS.empty, "Mary", "Susie"]
    it "ColvarLtDigit" $ do
      runSystem ("$3 < 20 { print $1 }") testAwkData
        `shouldBe` Right ["Beth", "Kathy", BS.empty, "Dan", BS.empty, "Susie"]
    it "ColvarLeDigit" $ do
      runSystem ("$3 <= 20 { print $1 }") testAwkData
        `shouldBe` Right ["Beth", "Kathy", "Mark", "Dan", BS.empty, "Susie"]
    it "ColvarGtDigit" $ do
      runSystem ("$3 > 20 { print $1 }") testAwkData
        `shouldBe` Right [BS.empty, BS.empty, BS.empty, BS.empty, "Mary", BS.empty]
    it "ColvarGeDigit" $ do
      runSystem ("$3 >= 20 { print $1 }") testAwkData
        `shouldBe` Right [BS.empty, BS.empty, "Mark", BS.empty, "Mary", BS.empty]
    it "ColvarEqColvarFlipped" $ do
      runSystem ("0 == $3 { print $1 }") testAwkData
        `shouldBe` Right ["Beth", BS.empty, BS.empty, "Dan", BS.empty, BS.empty]
    it "ColvarLeColvarFlipped" $ do
      runSystem ("20 <= $3 { print $1 }") testAwkData
        `shouldBe` Right [BS.empty, BS.empty, "Mark", BS.empty, "Mary", BS.empty]
    it "IntLtTrue" $ do
      runSystem ("0 < 1 { print $1 }") testAwkData
        `shouldBe` Right ["Beth", "Kathy", "Mark", "Dan", "Mary", "Susie"]

testPrint :: IO ()
testPrint = hspec $ do
  describe "testPrint" $ do
    it "Print1Colvar" $ do
      runSystem ("{ print $1 }") testAwkData
        `shouldBe` Right ["Beth", "Kathy", "Mark", "Dan", "Mary", "Susie"]
    it "PrintManyColvar" $ do
      runSystem ("{ print $1, $2, $3 }") testAwkData
        `shouldBe` Right (BS.lines testAwkData)
    it "PrintColvar0" $ do
      runSystem ("{ print $0, $1 }") testAwkData
        `shouldBe` Right ["Beth 4.00 0 Beth", "Kathy 4.00 10 Kathy", "Mark 5.00 20 Mark",
                          "Dan 3.75 0 Dan", "Mary 5.50 22 Mary", "Susie 4.25 18 Susie"]
    it "printEmpty" $ do
      runSystem ("{ print }") testAwkData
        `shouldBe` Right (BS.lines testAwkData)
