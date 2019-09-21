{-# LANGUAGE OverloadedStrings #-}

module Main where

import Schema
import Server
import Query

configFile :: FilePath
configFile = "server.cfg"

main :: IO ()
main = parseConfig configFile >>= runServer
