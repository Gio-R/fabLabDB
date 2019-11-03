{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Semigroup ((<>))
import Options.Applicative
import Query
import Schema
import Server

configFile :: FilePath
configFile = "server.cfg"

passwordParser :: Parser String
passwordParser = argument str (metavar "PASSWORD")

pswdInfo :: ParserInfo String
pswdInfo =
  info (passwordParser <**> helper)
    ( fullDesc
        <> progDesc "Starts the server application using the PASSWORD for connecting to the database"
      )

main :: IO ()
main = do
  pswd <- execParser pswdInfo
  parseConfig configFile >>= (flip runServer pswd)
