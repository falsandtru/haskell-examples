module LibSpec where

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"::String) #-}
{-# ANN module ("HLint: ignore Redundant $"::String) #-}
spec :: Spec
spec = do
    describe "list" $ do
        it "head" $
            head "abc" `shouldBe` 'a'