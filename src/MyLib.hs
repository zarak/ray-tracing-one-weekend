module MyLib (someFunc) where

import Data.Foldable (traverse_)

imageWidth :: Double
imageWidth = 256

imageHeight :: Double
imageHeight = 256

someFunc :: IO ()
someFunc = do
  putStrLn $ "P3\n" <> show imageWidth <> " " <> show imageHeight <> "\n255"
  traverse_ (\(ir, ig, ib) -> printFunc ir ig ib) genVals

printFunc :: Integer -> Integer -> Integer -> IO ()
printFunc ir ig ib = do
  putStrLn $ show ir <> " " <> show ig <> " " <> show ib

genVals :: [(Integer, Integer, Integer)]
genVals =
  let vals =
        [ [ ((255.999 * i) / (imageWidth - 1), 255.999 * j / (imageHeight - 1), 255.9999 / 4)
            | i <- [0 .. imageWidth - 1]
          ]
          | j <- [imageHeight, imageHeight - 1 .. 0]
        ] ::
          [[(Double, Double, Double)]]
   in map (\(x, y, z) -> (floor x, floor y, floor z)) $ concat vals
