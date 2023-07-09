{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Dhall (FromDhall, Natural)
import Dhall qualified
import GHC.Generics
import MyLib qualified (someFunc)
import Options.Applicative

data AppConfig = AppConfig
  { imageWidth :: Maybe Natural,
    samplesPerPixel :: Maybe Natural
  }
  deriving (Show, Eq, Generic, FromDhall)

main :: IO ()
main = do
  opts <- getOptions
  print opts
  MyLib.someFunc

configParser :: Parser AppConfig
configParser =
  AppConfig
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

handleOptions :: IO AppConfig
handleOptions =
  execParser $
    info
      (configParser <**> helper)
      ( fullDesc
          <> progDesc "Ray tracer"
      )

getOptions :: IO AppConfig
getOptions = do
  opts <- handleOptions
  case opts of
    AppConfig Nothing Nothing -> defaultOptions
    _ -> pure opts

defaultOptions :: IO AppConfig
defaultOptions = Dhall.input Dhall.auto "./app/config.dhall"
