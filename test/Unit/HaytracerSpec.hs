module Unit.HaytracerSpec (spec) where

import Haytracer (someFunc)
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

-- TODO add documentation

spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  describe "example assertion" $ do
    it "trivial" $
      someFunc `shouldBe` "someFunc"
  prop "example property" $
    \n -> n > (n - 1 :: Int)