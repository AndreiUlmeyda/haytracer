module Main (main) where

import Test.Hspec (hspec)
import Unit.HaytracerSpec as H
import Unit.TypesSpec as T

main :: IO ()
main = hspec $ do
  describe "FooSpec" H.spec
  describe "BarSpec" T.spec