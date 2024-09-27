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
  ( ThreeTuple (..),
    addTuple,
    epsilon,
    negateTuple,
    subtractTuple,
  )
import Prelude hiding
  ( negate,
    subtract,
  )

spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  describe "points and vectors" $ do
    let point = Point {x = 4.3, y = -4.2, z = 3.1}
    let vector = Vector {x = 4.3, y = -4.2, z = 3.1}

    it "access" $ do
      x point `shouldBe` 4.3
      y point `shouldBe` -4.2
      z point `shouldBe` 3.1
      x vector `shouldBe` 4.3
      y vector `shouldBe` -4.2
      z vector `shouldBe` 3.1
    -- Note: The book provides different directions for distinguishing points
    -- and vectors. There, they are the same type containing a fourth field
    -- whose content indicates the type. Up and until there is no way to avoid
    -- it, points and vectors will simply be represented by different types or
    -- at least type constructors.
    -- There is no point in implementing, or immediately obvious way to
    -- implement, an 'a point is not a vector' test. The type system takes care
    -- of that as it should. Watch me eat my words when the math hits.
    it "addition" $ do
      addTuple point vector `shouldBe` Just (Point {x = 8.6, y = -8.4, z = 6.2})
      addTuple point vector `shouldBe` addTuple vector point
      addTuple vector vector `shouldBe` Just (Vector {x = 8.6, y = -8.4, z = 6.2})
    it "subtraction" $ do
      -- TODO turn into property
      subtractTuple point point `shouldBe` Just (Vector {x = 0, y = 0, z = 0})
      subtractTuple vector vector `shouldBe` Just (Vector {x = 0, y = 0, z = 0})
      subtractTuple point vector `shouldBe` Just (Point {x = 0, y = 0, z = 0})
    it "negation" $ do
      -- TODO turn into property (adding to negative should give 0)
      negateTuple point `shouldBe` Point {x = -4.3, y = 4.2, z = -3.1}
      negateTuple vector `shouldBe` Vector {x = -4.3, y = 4.2, z = -3.1}

    it "comparison" $ do
      let signficantlyDifferentPoint = Point {x = 4.3 + 2 * epsilon, y = -4.2, z = 3.1}
      let marginallyDifferentPoint = Point {x = 4.3, y = -4.2 + epsilon / 2, z = 3.1}
      point `shouldNotBe` signficantlyDifferentPoint
      point `shouldBe` marginallyDifferentPoint
      let signficantlyDifferentVector = Vector {x = 4.3, y = -4.2, z = 3.1 - 3 * epsilon}
      let marginallyDifferentVector = Vector {x = 4.3, y = -4.2 + epsilon / 5, z = 3.1}
      vector `shouldNotBe` signficantlyDifferentVector
      vector `shouldBe` marginallyDifferentVector

  prop "example property" $
    \n -> n > (n - 1 :: Int)