{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Vec3Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Vec3 (Vec3 (..), cross, dot, scaleDiv, scaleVec3, unitToInterval, unitVector)

newtype Vec3' = Vec3'
  { toVec3 :: Vec3
  }
  deriving (Show)

instance Arbitrary Vec3' where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    pure $ Vec3' (Vec3 x y z)

spec :: Spec
spec = do
  describe "Vec3" $ do
    it "should add vectors correctly" $ do
      Vec3 1 2 3 + Vec3 4 5 6 `shouldBe` Vec3 5 7 9

    it "should subtract vectors correctly" $ do
      Vec3 1 2 3 - Vec3 4 5 6 `shouldBe` Vec3 (-3) (-3) (-3)

    it "should perform Hadamard product correctly" $ do
      Vec3 1 2 3 * Vec3 4 5 6 `shouldBe` Vec3 4 10 18

    it "should scale vectors correctly" $ do
      scaleVec3 2 (Vec3 1 2 3) `shouldBe` Vec3 2 4 6

    it "should divide vectors by scalar correctly" $ do
      scaleDiv (Vec3 2 4 6) 2 `shouldBe` Vec3 1 2 3

    prop "" $ do
      let propAbsSignum :: Vec3' -> Bool
          propAbsSignum (Vec3' (Vec3 x y z)) = all (\v -> abs v * signum v == v) [x, y, z]
      propAbsSignum

  describe "dot" $ do
    it "should compute dot product correctly" $ do
      dot (Vec3 1 2 3) (Vec3 4 5 6) `shouldBe` 32

  describe "cross" $ do
    it "should compute cross product correctly" $ do
      cross (Vec3 1 2 3) (Vec3 4 5 6) `shouldBe` Vec3 (-3) 6 (-3)

  describe "unitVector" $ do
    it "should compute unit vector correctly" $ do
      let v = Vec3 1 2 3
      let len = sqrt (dot v v)
      unitVector v `shouldBe` scaleDiv v len
  describe "unitToInterval" $ do
    it "maps (-1, -1, -1) to (0, 0, 0)" $
      unitToInterval (Vec3 (-1) (-1) (-1)) `shouldBe` Vec3 0 0 0

    it "maps (1, 1, 1) to (1, 1, 1)" $
      unitToInterval (Vec3 1 1 1) `shouldBe` Vec3 1 1 1
