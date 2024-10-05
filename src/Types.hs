-- |
-- Module      : Types
-- Description : Provide types, type classes and instances for point and vector
--               arithmetic.
-- Copyright   : (c) Adrian Schurz, 2024
-- License     : MIT
-- Maintainer  : schurz.adrian@mail.com
-- Stability   : experimental
module Types
  ( epsilon,
    ThreeTuple (..),
    addTuple,
    subtractTuple,
    negateTuple,
    scalarMultiply,
    scalarDivide,
    tupleMagnitude,
    tupleNormalize,
    dotProductTuple,
  )
where

-- | Represent tuples of spacial coordinates to represent points and vecors
--   in 3 dimensions. TODO switch to parametrized type to allow implementing
--   Functor and Foldable
data ThreeTuple
  = Point {x :: Double, y :: Double, z :: Double}
  | Vector {x :: Double, y :: Double, z :: Double}
  deriving stock (Show)

-- | Provide context in undefined situations or error cases
type Error = String

-- | Equality needs to be implemented rather than derived automatically
--   because the type contains floating point fields.
instance Eq ThreeTuple where
  (==) :: ThreeTuple -> ThreeTuple -> Bool
  (Point x1 y1 z1) == (Point x2 y2 z2) =
    absoluteDifferenceBelowThreshold x1 x2
      && absoluteDifferenceBelowThreshold y1 y2
      && absoluteDifferenceBelowThreshold z1 z2
  (Vector x1 y1 z1) == (Vector x2 y2 z2) =
    absoluteDifferenceBelowThreshold x1 x2
      && absoluteDifferenceBelowThreshold y1 y2
      && absoluteDifferenceBelowThreshold z1 z2
  _ == _ = False

-- | An arbitrary, small value for floating point comparison
epsilon :: Double
epsilon = 1e-11

toVector :: ThreeTuple -> ThreeTuple
toVector (Point a b c) = Vector a b c
toVector a = a

-- | Used as the definition of floating point equality
absoluteDifferenceBelowThreshold :: Double -> Double -> Bool
absoluteDifferenceBelowThreshold a b = abs (a - b) <= epsilon

applyElementWise :: (Double -> Double) -> ThreeTuple -> ThreeTuple
applyElementWise f t = t {x = (f . x) t, y = (f . y) t, z = (f . z) t}

combineElementWise :: (Double -> Double -> Double) -> ThreeTuple -> ThreeTuple -> ThreeTuple
combineElementWise f t1 t2 = t1 {x = f (x t1) (x t2), y = f (y t1) (y t2), z = f (z t1) (z t2)}

-- | Elementwise addition of tuples
addTuple :: ThreeTuple -> ThreeTuple -> Either ThreeTuple Error
addTuple a b
  | Point {} <- a, Point {} <- b = Right "adding a point to a point does not have meaning in this context"
  | Vector {} <- a, Point {} <- b = addTuple b a
  | otherwise = Left $ combineElementWise (+) a b

-- | Elementwise subtraction of tuples
subtractTuple :: ThreeTuple -> ThreeTuple -> Either ThreeTuple Error
subtractTuple a b
  | Vector {} <- a, Point {} <- b = Right "subtracting a point from a vector does not have meaning in this context"
  | Point {} <- a, Point {} <- b = Left $ toVector $ combineElementWise (-) a b
  | otherwise = Left $ combineElementWise (-) a b

-- | Elementwise negation of tuples
negateTuple :: ThreeTuple -> ThreeTuple
negateTuple = applyElementWise (0 -)

-- | Scalar multiplication of tuples
scalarMultiply :: Double -> ThreeTuple -> ThreeTuple
scalarMultiply factor = applyElementWise (* factor)

-- | Scalar division of tuples TODO disallow zero using types
scalarDivide :: ThreeTuple -> Double -> Either ThreeTuple Error
scalarDivide _ 0 = Right "division by zero is undefined"
scalarDivide t divisor = Left $ scalarMultiply (1 / divisor) t

-- | Compute the magnitude or length of a tuple
tupleMagnitude :: ThreeTuple -> Double
tupleMagnitude t = sqrt (x t ^ (2 :: Int) + y t ^ (2 :: Int) + z t ^ (2 :: Int))

-- | Tuple normalization / scaling to length 1 TODO disallow zero using types
tupleNormalize :: ThreeTuple -> Either ThreeTuple Error
tupleNormalize t
  | tupleMagnitude t == 0 = Right "tuples of magnitude zero cannot be normalized"
  | otherwise = Left $ applyElementWise (/ tupleMagnitude t) t

dotProductTuple :: ThreeTuple -> ThreeTuple -> Double
dotProductTuple t1 t2 = x t1 * x t2 + y t1 * y t2 + z t1 * z t2