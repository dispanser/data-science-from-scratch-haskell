import Lib (function1)
import Test.Hspec

main :: IO ()
main = hspec $ do
  spec

spec :: Spec
spec = do
  describe "basic spec description" $ do
    context "some more detailed context" $ do
      it "concrete test description" $ function1 [1 :: Int, 2, 3] `shouldBe` [3, 2, 1]


