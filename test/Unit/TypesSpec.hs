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
    scalarDivide,
    scalarMultiply,
    subtractTuple,
    tupleMagnitude,
  )
import Prelude hiding
  ( negate,
    subtract,
  )

-- TODO add documentation

spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  describe "three tuples" $ do
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
      addTuple point vector `shouldBe` Left (Point {x = 8.6, y = -8.4, z = 6.2})
      addTuple point vector `shouldBe` addTuple vector point
      addTuple vector vector `shouldBe` Left (Vector {x = 8.6, y = -8.4, z = 6.2})
      addTuple point point `shouldBe` Right "adding a point to a point does not have meaning in this context"
    prop "adding the zero vector to a vector should leave it unchanged" $
      \a b c -> addTuple (Vector a b c) zeroVector `shouldBe` Left (Vector a b c)
    prop "adding the zero vector to a point should leave the point unchanged" $
      \a b c -> addTuple (Point a b c) zeroVector `shouldBe` Left (Point a b c)
    prop "adding the zero vector to a vector should leave it unchanged" $
      \a b c -> addTuple (Vector a b c) zeroVector `shouldBe` Left (Vector a b c)

    prop "subtracting a point from itself should be the zero vector" $
      \a b c -> subtractTuple (Point a b c) (Point a b c) `shouldBe` Left zeroVector
    prop "subtracting a vector from itself should be the zero vector" $
      \a b c -> subtractTuple (Vector a b c) (Vector a b c) `shouldBe` Left zeroVector
    prop "subtracting a vector from an equivalent point should be the zero point" $
      \a b c -> subtractTuple (Point a b c) (Vector a b c) `shouldBe` Left zeroPoint
    it "subtracting a point from a vector yields nothing" $
      subtractTuple vector point `shouldBe` Right "subtracting a point from a vector does not have meaning in this context"

    prop "negation of a point" $
      \a b c -> negateTuple (Point a b c) `shouldBe` Point (-a) (-b) (-c)
    prop "negation of a vector" $
      \a b c -> negateTuple (Vector a b c) `shouldBe` Vector (-a) (-b) (-c)
    prop "negating twice should be the identity function for points" $
      \a b c -> (negateTuple . negateTuple) (Point a b c) `shouldBe` Point a b c
    prop "negating twice should be the identity function for vectors" $
      \a b c -> (negateTuple . negateTuple) (Vector a b c) `shouldBe` Vector a b c

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

    prop "scalar multiplication of a point" $
      \a b c factor -> scalarMultiply (Point a b c) factor `shouldBe` Point (a * factor) (b * factor) (c * factor)
    prop "scalar multiplication of a vector" $
      \a b c factor -> scalarMultiply (Vector a b c) factor `shouldBe` Vector (a * factor) (b * factor) (c * factor)

    prop "scalar division of a point" $
      let divisor = 3.5 :: Double
       in \a b c -> scalarDivide (Point a b c) divisor `shouldBe` Left (Point (a / divisor) (b / divisor) (c / divisor))
    prop "scalar division of a vector" $
      let divisor = 7.1 :: Double
       in \a b c -> scalarDivide (Vector a b c) divisor `shouldBe` Left (Vector (a / divisor) (b / divisor) (c / divisor))

    it "division by zero is undefined" $ do
      scalarDivide point 0 `shouldBe` Right "division by zero is undefined"
      scalarDivide vector 0 `shouldBe` Right "division by zero is undefined"

    it "magnitude of a point" $ do
      tupleMagnitude zeroPoint `shouldBe` 0
      tupleMagnitude (Point 4.1 0 0) `shouldBe` 4.1
      tupleMagnitude (Point 1 2 3) `shouldBe` sqrt 14
      tupleMagnitude (Point (-1) (-2) (-3)) `shouldBe` sqrt 14