{-# LANGUAGE NegativeLiterals #-}

module SphereSpec where

import Sphere
import Test.Hspec

spec :: Spec
spec = describe "findNearestRoot" $ do
  it "returns Nothing when no roots are in range" $ do
    findNearestRoot 2 2 1 (6, 8) `shouldBe` Nothing

  it "returns the first root if it is in range" $ do
    findNearestRoot 2.5 2.5 1 (-6, 0) `shouldBe` Just (-5)

  it "returns the second root if the first is not in range" $ do
    findNearestRoot 2.5 2.5 1 (-1, 5) `shouldBe` Just 0.0

  it "returns the first root if both are in range" $ do
    findNearestRoot 2.5 2.5 1 (-20, 20) `shouldBe` Just (-5)
