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
  deriving stock (Eq, Show)

data Vector = MkVector
  { vx :: Double,
    vy :: Double,
    vz :: Double
  }
  deriving stock (Eq, Show)