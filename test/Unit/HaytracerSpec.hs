module Unit.HaytracerSpec where

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
  describe "todo" $ do
    it "todo" $
      True `shouldBe` True
  prop "True" $
    \n -> n > (n - 1 :: Int)