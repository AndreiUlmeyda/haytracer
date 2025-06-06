module ProjectileExample () where

import Types
  ( Point (..),
    Vector (..),
  )

data Projectile = MkProjectile
  { position :: Point,
    veloctity :: Vector
  }