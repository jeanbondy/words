import Test.Hspec
import Lib
import Conf

main :: IO ()
main = hspec $ do
    describe "formatGrid" $ do
        it "should concatenate every line with a newline" $ do
            (formatGrid ["abc", "def", "ghi"]) `shouldBe` "abc\ndef\nghi\n"
    
    describe "findWord" $ do
      it "should find words that exist on the Grid" $ do
        findWord grid "HASKELL" `shouldBe` Just "HASKELL"
        findWord grid "PERL" `shouldBe` Just "PERL"
      it "should not find words that do not exist on the Grid" $ do
        findWord grid "HAMSTER" `shouldBe` Nothing
        
    describe "findWords" $ do
      it "should find all the words that exist on the grid" $ do
        findWords grid languages `shouldBe` languages
