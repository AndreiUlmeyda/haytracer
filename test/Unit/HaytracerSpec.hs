module Unit.HaytracerSpec (spec) where

import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )
import Test.Hspec.QuickCheck
  ( modifyMaxSuccess,
    prop,
  )

spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  describe "Example assertion" $ do
    it "trivial" $
      True `shouldBe` True
  prop "Example property" $
    \n -> n > (n - 1 :: Int)