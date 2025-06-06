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
    Point (..),
    Vector (..),
    addTuple,
    subtractTuple,
    negateTuple,
    scalarMultiply,
    scalarDivide,
    tupleMagnitude,
    tupleNormalize,
    dotProductTuple,
    crossProductTuple,
    addPoints,
    px,
    py,
    pz,
    vx,
    vy,
    vz,
  )
where

-- | Represent tuples of spacial coordinates to represent points and vecors
--  in 3 dimensions. TODO switch to parametrized type to allow implementing
--  Functor and Foldable
data ThreeTuple a = MkThreeTuple
  { x :: a,
    y :: a,
    z :: a
  }
  deriving stock (Show)

newtype Point = MkPoint (ThreeTuple Double)

newtype Vector = MkVector (ThreeTuple Double)

instance Eq Point where
  (==) :: Point -> Point -> Bool
  (MkPoint (MkThreeTuple x1 y1 z1)) == (MkPoint (MkThreeTuple x2 y2 z2)) =
    absoluteDifferenceBelowThreshold x1 x2
      && absoluteDifferenceBelowThreshold y1 y2
      && absoluteDifferenceBelowThreshold z1 z2

instance Eq Vector where
  (==) :: Vector -> Vector -> Bool
  (MkVector (MkThreeTuple x1 y1 z1)) == (MkVector (MkThreeTuple x2 y2 z2)) =
    absoluteDifferenceBelowThreshold x1 x2
      && absoluteDifferenceBelowThreshold y1 y2
      && absoluteDifferenceBelowThreshold z1 z2

px :: Point -> Double
px (MkPoint (MkThreeTuple x _ _)) = x

py :: Point -> Double
py (MkPoint (MkThreeTuple _ y _)) = y

pz :: Point -> Double
pz (MkPoint (MkThreeTuple _ _ z)) = z

vx :: Vector -> Double
vx (MkVector (MkThreeTuple x _ _)) = x

vy :: Vector -> Double
vy (MkVector (MkThreeTuple _ y _)) = y

vz :: Vector -> Double
vz (MkVector (MkThreeTuple _ _ z)) = z

-- | Provide context in undefined situations or error cases
type Error = String

-- | An arbitrary, small value for floating point comparison
epsilon :: Double
epsilon = 1e-11

toVector :: Point -> Vector
toVector (MkPoint t) = MkVector t

-- | Used as the definition of floating point equality
absoluteDifferenceBelowThreshold :: Double -> Double -> Bool
absoluteDifferenceBelowThreshold a b = abs (a - b) <= epsilon

applyElementWise :: (a -> a) -> ThreeTuple a -> ThreeTuple a
applyElementWise f t = t {x = (f . x) t, y = (f . y) t, z = (f . z) t}

combineElementWise :: (a -> a -> a) -> ThreeTuple a -> ThreeTuple a -> ThreeTuple a
combineElementWise f (MkThreeTuple x1 y1 z1) (MkThreeTuple x2 y2 z2) = MkThreeTuple {x = f x1 x2, y = f y1 y2, z = f z1 z2}

-- | Elementwise addition of tuples
addTuple :: (Num a) => ThreeTuple a -> ThreeTuple a -> ThreeTuple a
addTuple = combineElementWise (+)

addPoints :: Point -> Point -> Point
addPoints (MkPoint t1) (MkPoint t2) = MkPoint $ addTuple t1 t2

-- | Elementwise subtraction of tuples
subtractTuple :: (Num a) => ThreeTuple a -> ThreeTuple a -> ThreeTuple a
subtractTuple = combineElementWise (-)

-- | Elementwise negation of tuples
negateTuple :: (Num a) => ThreeTuple a -> ThreeTuple a
negateTuple = applyElementWise (0 -)

-- | Scalar multiplication of tuples
scalarMultiply :: (Num a) => a -> ThreeTuple a -> ThreeTuple a
scalarMultiply factor = applyElementWise (* factor)

-- | Scalar division of tuples TODO disallow zero using types
scalarDivide :: (Eq a, Fractional a) => ThreeTuple a -> a -> Either (ThreeTuple a) Error
scalarDivide _ 0 = Right "division by zero is undefined"
scalarDivide t divisor = Left $ scalarMultiply (1 / divisor) t

-- | Compute the magnitude or length of a tuple
tupleMagnitude :: (Floating a) => ThreeTuple a -> a
tupleMagnitude t = sqrt (x t ^ (2 :: Int) + y t ^ (2 :: Int) + z t ^ (2 :: Int))

-- | Tuple normalization / scaling to length 1 TODO disallow zero using types
tupleNormalize :: (Floating a, Eq a) => ThreeTuple a -> Either (ThreeTuple a) Error
tupleNormalize t
  | tupleMagnitude t == 0 = Right "tuples of magnitude zero cannot be normalized"
  | otherwise = Left $ applyElementWise (/ tupleMagnitude t) t

dotProductTuple :: (Num a) => ThreeTuple a -> ThreeTuple a -> a
dotProductTuple t1 t2 = x t1 * x t2 + y t1 * y t2 + z t1 * z t2

crossProductTuple :: (Num a) => ThreeTuple a -> ThreeTuple a -> ThreeTuple a
crossProductTuple t1 t2 =
  MkThreeTuple
    { x = y t1 * z t2 - z t1 * y t2,
      y = z t1 * x t2 - x t1 * z t2,
      z = x t1 * y t2 - y t1 * x t2
    }