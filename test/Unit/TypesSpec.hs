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
    let zeroPoint = Point {x = 0, y = 0, z = 0}
    let zeroVector = Vector {x = 0, y = 0, z = 0}


    it "access" $ do
      x point `shouldBe` 4.3
      y point `shouldBe` -4.2
      z point `shouldBe` 3.1
      x vector `shouldBe` 4.3
      y vector `shouldBe` -4.2
      z vector `shouldBe` 3.1
    it "addition" $ do
      addTuple point vector `shouldBe` Just (Point {x = 8.6, y = -8.4, z = 6.2})
      addTuple point vector `shouldBe` addTuple vector point
      addTuple vector vector `shouldBe` Just (Vector {x = 8.6, y = -8.4, z = 6.2})
      addTuple point point `shouldBe` Nothing
    prop "adding the zero tuple should leave it unchanged" $ do
      \a b c -> addTuple (Vector a b c) zeroVector `shouldBe` Just (Vector a b c)
      \a b c -> addTuple (Point a b c) zeroVector `shouldBe` Just (Point a b c)
      \a b c -> addTuple (Vector a b c) zeroVector `shouldBe` Just (Vector a b c)
    it "subtraction" $ do
      -- TODO turn into property
      subtractTuple point point `shouldBe` Just zeroVector
      subtractTuple vector vector `shouldBe` Just zeroVector
      subtractTuple point vector `shouldBe` Just zeroPoint
      subtractTuple vector point `shouldBe` Nothing
    prop "negation" $ do
      \a b c -> negateTuple (Point a b c) `shouldBe` Point (-a) (-b) (-c)
      \a b c -> negateTuple (Vector a b c) `shouldBe` Vector (-a) (-b) (-c)

    it "comparison" $ do
      let signficantlyDifferentPoint = Point {x = 4.3 + 2 * epsilon, y = -4.2, z = 3.1}
      let marginallyDifferentPoint = Point {x = 4.3, y = -4.2 + epsilon / 2, z = 3.1}
      point `shouldNotBe` signficantlyDifferentPoint
      point `shouldBe` marginallyDifferentPoint
      let signficantlyDifferentVector = Vector {x = 4.3, y = -4.2, z = 3.1 - 3 * epsilon}
      let marginallyDifferentVector = Vector {x = 4.3, y = -4.2 + epsilon / 5, z = 3.1}
      vector `shouldNotBe` signficantlyDifferentVector
      vector `shouldBe` marginallyDifferentVector
      vector `shouldNotBe` point
    it "conversion to string" $ do
      show point `shouldBe` "Point {x = 4.3, y = -4.2, z = 3.1}"
      show vector `shouldBe` "Vector {x = 4.3, y = -4.2, z = 3.1}"
      

  prop "example property" $
    \n -> n > (n - 1 :: Int)