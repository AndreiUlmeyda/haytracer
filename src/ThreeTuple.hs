module ThreeTuple
  ( ThreeTuple (..),
  )
where

data ThreeTuple = MkThreeTuple
  { x :: Double,
    y :: Double,
    z :: Double
  }
  deriving stock (Eq, Show)