module Unit.ThreeTupleSpec (spec) where

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
import ThreeTuple
  ( ThreeTuple (..),
  )

spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  describe "ThreeTuple" $ do
    let simpleTuple = MkThreeTuple {x = 4.3, y = -4.2, z = 3.1}
    let simpleTuple' = MkThreeTuple 4.3 (-4.2) 3.1
    it "creation" $
      simpleTuple `shouldBe` simpleTuple'
    it "access" $ do
      x simpleTuple `shouldBe` 4.3
      y simpleTuple `shouldBe` -4.2
      z simpleTuple `shouldBe` 3.1

  prop "Example property" $
    \n -> n > (n - 1 :: Int)