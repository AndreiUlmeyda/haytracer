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
  describe "example assertion" $ do
    it "trivial" $
      True `shouldBe` True
  prop "example property" $
    \n -> n > (n - 1 :: Int)