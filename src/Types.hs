{-# LANGUAGE InstanceSigs #-}

module Types
  ( epsilon,
    ThreeTuple (..),
    addTuple,
    subtractTuple,
    negateTuple,
  )
where

data ThreeTuple
  = Point {x :: Double, y :: Double, z :: Double}
  | Vector {x :: Double, y :: Double, z :: Double}
  deriving stock (Show)

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

epsilon :: Double
epsilon = 1e-9

absoluteDifferenceBelowThreshold :: Double -> Double -> Bool
absoluteDifferenceBelowThreshold a b = abs (a - b) <= epsilon

-- TODO swith to Either to provide error messages
addTuple :: ThreeTuple -> ThreeTuple -> Maybe ThreeTuple
addTuple a b
  | (Point x1 y1 z1) <- a, (Vector x2 y2 z2) <- b = Just $ Point {x = x1 + x2, y = y1 + y2, z = z1 + z2}
  | Vector {} <- a, Point {} <- b = addTuple b a
  | (Vector x1 y1 z1) <- a, (Vector x2 y2 z2) <- b = Just $ Vector {x = x1 + x2, y = y1 + y2, z = z1 + z2}
  | Point {} <- a, Point {} <- b = Nothing

-- TODO swith to Either to provide error messages
subtractTuple :: ThreeTuple -> ThreeTuple -> Maybe ThreeTuple
subtractTuple a b
  | (Point x1 y1 z1) <- a, (Point x2 y2 z2) <- b = Just $ Vector {x = x1 - x2, y = y1 - y2, z = z1 - z2}
  | (Point x1 y1 z1) <- a, (Vector x2 y2 z2) <- b = Just $ Point {x = x1 - x2, y = y1 - y2, z = z1 - z2}
  | (Vector x1 y1 z1) <- a, (Vector x2 y2 z2) <- b = Just $ Vector {x = x1 - x2, y = y1 - y2, z = z1 - z2}
  | (Vector x1 y1 z1) <- a, (Point x2 y2 z2) <- b = Nothing

negateTuple :: ThreeTuple -> ThreeTuple
negateTuple a = a {x = -(x a), y = -(y a), z = -(z a)}