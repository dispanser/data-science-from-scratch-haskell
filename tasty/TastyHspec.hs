-- |
-- contains @hspec@ definitions that will be used from tasty main.
--
-- Note that it doesn't contain any reference to tasty directly, so it could be
-- used independently from both a spec and a tasty test suite.
--
module TastyHspec (
    rootSpec)

  where

import Lib (function1)
import Test.Hspec

rootSpec :: Spec
rootSpec = do
  describe "basic spec description" $ do
    context "some more detailed context" $ do
      it "concrete test description" $ function1 [1 :: Int, 2, 3] `shouldBe` [3, 2, 1]
