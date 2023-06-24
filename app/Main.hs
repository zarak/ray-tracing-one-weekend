module Main where

import MyLib qualified (someFunc)

-- import System.Environment (getArgs)

main :: IO ()
main = do
  -- args <- getArgs
  -- let width : _ = read <$> args
  MyLib.someFunc
