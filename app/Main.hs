{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Dhall (FromDhall, Natural)
import Dhall qualified
import GHC.Generics
import MyLib qualified (someFunc)
import Options.Applicative

data Options = Options
  { imageWidth :: Maybe Natural,
    samplesPerPixel :: Maybe Natural
  }
  deriving (Show, Eq, Generic, FromDhall)

main :: IO ()
main = do
  opts <- getOptions
  print opts
  MyLib.someFunc

optionsParser :: Parser Options
optionsParser =
  Options
    <$> optional
      ( option
          auto
          ( long "imageWidth"
              <> help "Set the image width"
              <> metavar "INT"
          )
      )
    <*> optional
      ( option
          auto
          ( long "samplesPerPixel"
              <> help "Set the samples per pixel"
              <> metavar "INT"
          )
      )

handleOptions :: IO Options
handleOptions =
  execParser $
    info
      (optionsParser <**> helper)
      ( fullDesc
          <> progDesc "Ray tracer"
      )

getOptions :: IO Options
getOptions = do
  opts <- handleOptions
  case opts of
    Options Nothing Nothing -> defaultOptions
    _ -> pure opts

defaultOptions :: IO Options
defaultOptions = Dhall.input Dhall.auto "./app/config.dhall"
