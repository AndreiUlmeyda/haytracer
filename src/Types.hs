{-# LANGUAGE InstanceSigs #-}

module Types
  ( Point (..),
    Vector (..),
    epsilon,
  )
where

data Point = MkPoint
  { px :: Double,
    py :: Double,
    pz :: Double
  }
  deriving stock (Show)

data Vector = MkVector
  { vx :: Double,
    vy :: Double,
    vz :: Double
  }
  deriving stock (Show)

epsilon :: Double
epsilon = 1e-9

instance Eq Point where
  (==) :: Point -> Point -> Bool
  (MkPoint x1 y1 z1) == (MkPoint x2 y2 z2) =
    abs (x1 - x2) <= epsilon
      && abs (y1 - y2) <= epsilon
      && abs (z1 - z2) <= epsilon

instance Eq Vector where
  (==) :: Vector -> Vector -> Bool
  (MkVector x1 y1 z1) == (MkVector x2 y2 z2) =
    abs (x1 - x2) <= epsilon
      && abs (y1 - y2) <= epsilon
      && abs (z1 - z2) <= epsilon