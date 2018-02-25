module LibSpec where

import Test.Hspec

spec :: Spec
spec = do
    describe "list" $ do
        it "head" $ do
            head "abc" `shouldBe` 'a'
