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
    let simplePoint = Point {x = 4.3, y = -4.2, z = 3.1}
    let simplePoint' = Point 4.3 (-4.2) 3.1
    let simpleVector = Vector {x = 4.3, y = -4.2, z = 3.1}
    let simpleVector' = Vector 4.3 (-4.2) 3.1

    it "creation" $ do
      simplePoint `shouldBe` simplePoint'
      simpleVector `shouldBe` simpleVector'

    it "access" $ do
      x simplePoint `shouldBe` 4.3
      y simplePoint `shouldBe` -4.2
      z simplePoint `shouldBe` 3.1
      x simpleVector `shouldBe` 4.3
      y simpleVector `shouldBe` -4.2
      z simpleVector `shouldBe` 3.1
    -- Note: The book provides different directions for distinguishing points
    -- and vectors. There, they are the same type containing a fourth field
    -- whose content indicates the type. Up and until there is no way to avoid
    -- it, points and vectors will simply be represented by different types or
    -- at least type constructors.
    -- There is no point in implementing, or immediately obvious way to
    -- implement, an 'a point is not a vector' test. The type system takes care
    -- of that as it should. Watch me eat my words when the math hits.
    it "addition" $ do
      addTuple simplePoint simpleVector `shouldBe` Just (Point {x = 8.6, y = -8.4, z = 6.2})
      addTuple simplePoint simpleVector `shouldBe` addTuple simpleVector simplePoint
      addTuple simpleVector simpleVector `shouldBe` Just (Vector {x = 8.6, y = -8.4, z = 6.2})
    it "subtraction" $ do
      -- TODO turn into property
      subtractTuple simplePoint simplePoint `shouldBe` Just (Vector {x = 0, y = 0, z = 0})
      subtractTuple simpleVector simpleVector `shouldBe` Just (Vector {x = 0, y = 0, z = 0})
      subtractTuple simplePoint simpleVector `shouldBe` Just (Point {x = 0, y = 0, z = 0})
    it "negation" $ do
      -- TODO turn into property (adding to negative should give 0)
      negateTuple simplePoint `shouldBe` Point {x = -4.3, y = 4.2, z = -3.1}
      negateTuple simpleVector `shouldBe` Vector {x = -4.3, y = 4.2, z = -3.1}

    it "comparison" $ do
      let signficantlyDifferentPoint = Point {x = 4.3 + 2 * epsilon, y = -4.2, z = 3.1}
      let marginallyDifferentPoint = Point {x = 4.3, y = -4.2 + epsilon / 2, z = 3.1}
      simplePoint `shouldNotBe` signficantlyDifferentPoint
      simplePoint `shouldBe` marginallyDifferentPoint
      let signficantlyDifferentVector = Vector {x = 4.3, y = -4.2, z = 3.1 - 3 * epsilon}
      let marginallyDifferentVector = Vector {x = 4.3, y = -4.2 + epsilon / 5, z = 3.1}
      simpleVector `shouldNotBe` signficantlyDifferentVector
      simpleVector `shouldBe` marginallyDifferentVector

  prop "example property" $
    \n -> n > (n - 1 :: Int)