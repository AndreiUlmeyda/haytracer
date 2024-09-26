{-# LANGUAGE InstanceSigs #-}

module Types
  ( Point (..),
    Vector (..),
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

instance Eq Point where
  (==) :: Point -> Point -> Bool
  (MkPoint x1 y1 z1) == (MkPoint x2 y2 z2) =
    x1 == x2 && y1 == y2 && z1 == z2

instance Eq Vector where
  (==) :: Vector -> Vector -> Bool
  (MkVector x1 y1 z1) == (MkVector x2 y2 z2) =
    x1 == x2 && y1 == y2 && z1 == z2