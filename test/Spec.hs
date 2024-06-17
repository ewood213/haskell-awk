import Test.Hspec
import Runner (runProgram)
import Parser (parse)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

testAwkData :: ByteString
testAwkData = "Beth 4.00 0\n\
\Dan 3.75 0\n\
\Kathy 4.00 10\n\
\Mark 5.00 20\n\
\Mary 5.50 22\n\
\Susie 4.25 18\n"

runSystem :: ByteString -> ByteString -> Either String ByteString
runSystem script input = do
  parsed <- parse script
  pure $ runProgram parsed input

main :: IO ()
main = do
  testColvarBinop
  testPrint
  testMultipleExp
  testStringPrinting
  testNF
  testNR
  testBeginEnd
  testVariables

testColvarBinop :: IO ()
testColvarBinop = hspec $ do
  describe "testColvarBinop" $ do
    it "ColvarEqDigit" $ do
      runSystem "$3 == 0 { print $1 }" testAwkData
        `shouldBe` Right "Beth\nDan\n"
    it "ColvarNeDigit" $ do
      runSystem "$3 != 0 { print $1 }" testAwkData
        `shouldBe` Right "Kathy\nMark\nMary\nSusie\n"
    it "ColvarLtDigit" $ do
      runSystem "$3 < 20 { print $1 }" testAwkData
        `shouldBe` Right "Beth\nDan\nKathy\nSusie\n"
    it "ColvarLeDigit" $ do
      runSystem "$3 <= 20 { print $1 }" testAwkData
        `shouldBe` Right "Beth\nDan\nKathy\nMark\nSusie\n"
    it "ColvarGtDigit" $ do
      runSystem "$3 > 20 { print $1 }" testAwkData
        `shouldBe` Right "Mary\n"
    it "ColvarGeDigit" $ do
      runSystem "$3 >= 20 { print $1 }" testAwkData
        `shouldBe` Right "Mark\nMary\n"
    it "ColvarEqColvarFlipped" $ do
      runSystem "0 == $3 { print $1 }" testAwkData
        `shouldBe` Right "Beth\nDan\n"
    it "ColvarLeColvarFlipped" $ do
      runSystem "20 <= $3 { print $1 }" testAwkData
        `shouldBe` Right "Mark\nMary\n"
    it "IntLtTrue" $ do
      runSystem "0 < 1 { print $1 }" testAwkData
        `shouldBe` Right "Beth\nDan\nKathy\nMark\nMary\nSusie\n"
    it "ColvarEqString" $ do
      runSystem "$1 == \"Beth\" { print $1 }" testAwkData
        `shouldBe` Right "Beth\n"
    it "TestColvar0" $ do
      runSystem "$0 > 1 { print $0 }" "0\n1\n2\n3\n4\n"
        `shouldBe` Right "2\n3\n4\n"
    it "TestColvarOutOfBounds" $ do
      runSystem "$5 == 0 { print $1 }" testAwkData
        `shouldBe` Right ""
    it "TestColvarOutOfBoundsVal" $ do
      runSystem "$5 == \"\" { print }" testAwkData
        `shouldBe` Right testAwkData
    it "TestColvarBinOps" $ do
      runSystem "!( $2 < 4 && $3 < 20) { print $1 }" testAwkData
        `shouldBe` Right "Beth\nKathy\nMark\nMary\nSusie\n"

testPrint :: IO ()
testPrint = hspec $ do
  describe "testPrint" $ do
    it "Print1Colvar" $ do
      runSystem "{ print $1 }" testAwkData
        `shouldBe` Right "Beth\nDan\nKathy\nMark\nMary\nSusie\n"
    it "PrintManyColvar" $ do
      runSystem "{ print $1, $2, $3 }" testAwkData
        `shouldBe` Right testAwkData
    it "PrintColvar0" $ do
      runSystem "{ print $0, $1 }" testAwkData
        `shouldBe` Right "Beth 4.00 0 Beth\nDan 3.75 0 Dan\nKathy 4.00 10 Kathy\n\
                          \Mark 5.00 20 Mark\nMary 5.50 22 Mary\nSusie 4.25 18 Susie\n"
    it "PrintInt" $ do
      runSystem "{ print 1, 5 }" testAwkData
        `shouldBe` Right "1 5\n1 5\n1 5\n1 5\n1 5\n1 5\n"
    it "printEmpty" $ do
      runSystem "{ print }" testAwkData
        `shouldBe` Right testAwkData
    it "printTimesExp" $ do
      runSystem "$3 > 0 { print $1, $2 * $3}" testAwkData
        `shouldBe` Right "Kathy 40\nMark 100\nMary 121\nSusie 76.5\n"


testMultipleExp :: IO ()
testMultipleExp = hspec $ do
  describe "testMultipleExp" $ do
    it "2Exp" $ do
      runSystem "$3 == 0 { print $1 } $3 == 10 { print $2 }" testAwkData
        `shouldBe` Right "Beth\nDan\n4.00\n"
    it "3Exp" $ do
      runSystem "{ print } { print } { print }" testAwkData
        `shouldBe` Right "Beth 4.00 0\nBeth 4.00 0\nBeth 4.00 0\n\
                         \Dan 3.75 0\nDan 3.75 0\nDan 3.75 0\n\
                         \Kathy 4.00 10\nKathy 4.00 10\nKathy 4.00 10\n\
                         \Mark 5.00 20\nMark 5.00 20\nMark 5.00 20\n\
                         \Mary 5.50 22\nMary 5.50 22\nMary 5.50 22\n\
                         \Susie 4.25 18\nSusie 4.25 18\nSusie 4.25 18\n"
    it "0Exp" $ do
      runSystem "" testAwkData
        `shouldBe` Right ""
    it "onlyExp" $ do
        runSystem "0==0" testAwkData
            `shouldBe` Right testAwkData

testStringPrinting :: IO ()
testStringPrinting = hspec $ do
  describe "testStringPrinting" $ do
    it "StringPrint" $ do
      runSystem "{ print \"Hello\" }" testAwkData
        `shouldBe` Right "Hello\nHello\nHello\nHello\nHello\nHello\n"
    it "StringPrintColvar" $ do
      runSystem "$3 == 0 { print \"Hello\", $1 }" testAwkData
        `shouldBe` Right "Hello Beth\nHello Dan\n"
    it "StringPrintEscapedQuotes" $ do
      runSystem "{ print \"Hello\", \"\\\"World\\\"\" }" testAwkData
        `shouldBe` Right "Hello \"World\"\nHello \"World\"\nHello \"World\"\n\
                          \Hello \"World\"\nHello \"World\"\nHello \"World\"\n"
    it "StringPrintEscapedNewline" $ do
      runSystem "{ print \"Hello\\nWorld\" }" testAwkData
        `shouldBe` Right "Hello\nWorld\nHello\nWorld\nHello\nWorld\n\
                          \Hello\nWorld\nHello\nWorld\nHello\nWorld\n"
    it "TotalPayPrint" $ do
      runSystem "{print \"total pay for\", $1, \"is\", $2 * $3}" testAwkData
        `shouldBe` Right "total pay for Beth is 0\n\
                         \total pay for Dan is 0\n\
                         \total pay for Kathy is 40\n\
                         \total pay for Mark is 100\n\
                         \total pay for Mary is 121\n\
                         \total pay for Susie is 76.5\n"
    -- TODO: (possibly by adding a TokenFail String to the Token type)
    -- it "StringPrintBadEscape" $ do
    --   runSystem "{ print \"Hello\\mWorld\" }" testAwkData
    --     `shouldBe` Left "Bad escape sequence: \\m\n"

testNF :: IO ()
testNF = hspec $ do
  describe "testNF" $ do
      it "testNF1" $ do
          runSystem "{ print NF }" testAwkData
            `shouldBe` Right "3\n3\n3\n3\n3\n3\n"
      it "testNF2" $ do
        runSystem "NF == 3 {print}" testAwkData
          `shouldBe` Right testAwkData
      it "testNF3" $ do
        runSystem "$NF == 0 {print $1}" testAwkData
          `shouldBe` Right "Beth\nDan\n"

testNR :: IO ()
testNR = hspec $ do
  describe "testNR" $ do
    it "testNR1" $ do
      runSystem "{ print NR }" testAwkData
        `shouldBe` Right "1\n2\n3\n4\n5\n6\n"
    it "testNR2" $ do
        runSystem "NR < 3 {print $1}" testAwkData
          `shouldBe` Right "Beth\nDan\n"
    it "testNR3" $ do
      runSystem "{print NR, $0 }" testAwkData
        `shouldBe` Right "1 Beth 4.00 0\n\
                    \2 Dan 3.75 0\n\
                    \3 Kathy 4.00 10\n\
                    \4 Mark 5.00 20\n\
                    \5 Mary 5.50 22\n\
                    \6 Susie 4.25 18\n"

testBeginEnd :: IO ()
testBeginEnd = hspec $ do
  describe "testBeginEnd" $ do
    it "testBegin" $ do
      runSystem "BEGIN {print \"HELLO!\"} {print}"  testAwkData
        `shouldBe` Right ("HELLO!\n" <> testAwkData)
    it "testBeginContents" $ do
      runSystem "BEGIN {print}" testAwkData
        `shouldBe` Right "\n"
    it "testBeginNRNF" $ do
      runSystem "BEGIN {print NR, NF}" testAwkData
        `shouldBe` Right "0 0\n"
    it "testEnd" $ do
      runSystem "END {print \"GOODBYE!\"} {print}" testAwkData
        `shouldBe` Right (testAwkData <> "GOODBYE!\n")
    it "testEndContents" $ do
      runSystem "END {print}" testAwkData
        `shouldBe` Right "Susie 4.25 18\n"
    it "testEndOrdering" $ do
      runSystem "END {print \"HEY\"} {print}" testAwkData
        `shouldBe` Right (testAwkData <> "HEY\n")
    it "testEndNRNF" $ do
      runSystem "END {print NR, NF}" testAwkData
        `shouldBe` Right "6 3\n"
    it "testBeginEndTogether" $ do
      runSystem "BEGIN {print \"HELLO\"} END {print \"GOODBYE\"} {print}" testAwkData
        `shouldBe` Right("HELLO\n" <> testAwkData <> "GOODBYE\n")

testVariables :: IO ()
testVariables = hspec $ do
  describe "testVariables" $ do
    it "testVariablesSimple" $ do
      runSystem "{var = \"Hello\"; print var }" "\n"
        `shouldBe` Right "Hello\n"
    it "testSumVariable" $ do
      runSystem "{sum += $3} END {print sum}" testAwkData
        `shouldBe` Right "70\n"
    it "testProductVariable" $ do
      runSystem "BEGIN {prod = 1} {prod *= $2} END {print prod}" testAwkData
        `shouldBe` Right "7012.5\n"
    it "testAverageVariable" $ do
      runSystem "{sum += $2} END {print sum/NR}" testAwkData
        `shouldBe` Right (BS.pack (show ((26.5:: Float) / 6)) <> "\n")
    it "testCountingVariable" $ do
      runSystem "$3 > 15 { emp += 1} END { print emp, \"employees worked over 15 hours\"}" testAwkData
        `shouldBe` Right "3 employees worked over 15 hours\n"
    it "testMax" $ do
      runSystem "$2 > maxrate { maxrate = $2; maxemp = $1 } END {print maxemp, maxrate}" testAwkData
        `shouldBe` Right "Mary 5.50\n"
