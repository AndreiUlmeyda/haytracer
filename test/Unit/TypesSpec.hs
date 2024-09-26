module Unit.TypesSpec (spec) where

import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
    shouldNotBe,
  )
import Test.Hspec.QuickCheck
  ( modifyMaxSuccess,
    prop,
  )
import Types
  ( Point (..),
    Vector (..),
    epsilon,
  )

spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  describe "points and vectors" $ do
    let simplePoint = MkPoint {px = 4.3, py = -4.2, pz = 3.1}
    let simplePoint' = MkPoint 4.3 (-4.2) 3.1
    let simpleVector = MkVector {vx = 4.3, vy = -4.2, vz = 3.1}
    let simpleVector' = MkVector 4.3 (-4.2) 3.1

    it "creation" $ do
      simplePoint `shouldBe` simplePoint'
      simpleVector `shouldBe` simpleVector'

    it "access" $ do
      px simplePoint `shouldBe` 4.3
      py simplePoint `shouldBe` -4.2
      pz simplePoint `shouldBe` 3.1
      vx simpleVector `shouldBe` 4.3
      vy simpleVector `shouldBe` -4.2
      vz simpleVector `shouldBe` 3.1
    -- Note: The book provides different directions for distinguishing points
    -- and vectors. There, they are the same type containing a fourth field
    -- whose content indicates the type. Up and until there is no way to avoid
    -- it, points and vectors will simply be represented by different types.
    -- There is no point in implementing, or immediately obvious way to
    -- implement, an 'a point is not a vector' test. The type system takes care
    -- of that as it should. Watch me eat my words when the math hits.

    it "comparison" $ do
      let signficantlyDifferentPoint = MkPoint {px = 4.3 + 2 * epsilon, py = -4.2, pz = 3.1}
      let marginallyDifferentPoint = MkPoint {px = 4.3, py = -4.2 + epsilon / 2, pz = 3.1}
      simplePoint `shouldNotBe` signficantlyDifferentPoint
      simplePoint `shouldBe` marginallyDifferentPoint
      let signficantlyDifferentVector = MkVector {vx = 4.3, vy = -4.2, vz = 3.1 - 3 * epsilon}
      let marginallyDifferentVector = MkVector {vx = 4.3, vy = -4.2 + epsilon / 5, vz = 3.1}
      simpleVector `shouldNotBe` signficantlyDifferentVector
      simpleVector `shouldBe` marginallyDifferentVector

  prop "example property" $
    \n -> n > (n - 1 :: Int)