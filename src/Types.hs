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
  )
where

-- | Represent tuples of spacial coordinates to represent points and vecors
--   in 3 dimensions.
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

-- | Used as the definition of floating point equality
absoluteDifferenceBelowThreshold :: Double -> Double -> Bool
absoluteDifferenceBelowThreshold a b = abs (a - b) <= epsilon

-- | Elementwise addition of tuples
addTuple :: ThreeTuple -> ThreeTuple -> Either ThreeTuple Error
addTuple a b
  | (Point x1 y1 z1) <- a, (Vector x2 y2 z2) <- b = Left $ Point {x = x1 + x2, y = y1 + y2, z = z1 + z2}
  | Vector {} <- a, Point {} <- b = addTuple b a
  | (Vector x1 y1 z1) <- a, (Vector x2 y2 z2) <- b = Left $ Vector {x = x1 + x2, y = y1 + y2, z = z1 + z2}
  | Point {} <- a, Point {} <- b = Right "adding a point to a point does not have meaning in this context"

-- | Elementwise subtraction of tuples
subtractTuple :: ThreeTuple -> ThreeTuple -> Either ThreeTuple Error
subtractTuple a b
  | (Point x1 y1 z1) <- a, (Point x2 y2 z2) <- b = Left $ Vector {x = x1 - x2, y = y1 - y2, z = z1 - z2}
  | (Point x1 y1 z1) <- a, (Vector x2 y2 z2) <- b = Left $ Point {x = x1 - x2, y = y1 - y2, z = z1 - z2}
  | (Vector x1 y1 z1) <- a, (Vector x2 y2 z2) <- b = Left $ Vector {x = x1 - x2, y = y1 - y2, z = z1 - z2}
  | Vector {} <- a, Point {} <- b = Right "subtracting a point from a vector does not have meaning in this context"

-- | Elementwise negation of tuples
negateTuple :: ThreeTuple -> ThreeTuple
negateTuple t = t {x = -(x t), y = -(y t), z = -(z t)}

-- | Scalar multiplication of tuples
scalarMultiply :: ThreeTuple -> Double -> ThreeTuple
scalarMultiply t factor = t {x = factor * x t, y = factor * y t, z = factor * z t}

-- | Scalar division of tuples
scalarDivide :: ThreeTuple -> Double -> Either ThreeTuple Error
scalarDivide _ 0 = Right "division by zero is undefined"
scalarDivide t divisor = Left $ scalarMultiply t (1 / divisor)

-- | Compute the magnitude or length of a tuple
tupleMagnitude :: ThreeTuple -> Double
tupleMagnitude t = sqrt (x t ^ (2 :: Int) + y t ^ (2 :: Int) + z t ^ (2 :: Int))
