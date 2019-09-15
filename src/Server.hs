{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |Module used for the connectivity part of the application
module Server where

import Control.Exception
import Control.Monad.Trans.Maybe
import Data.Aeson hiding (json)
import qualified Data.Configurator as C
import Data.HVect
import Data.Text
import Data.Text.Encoding
import Database.Beam
import Database.Beam.Postgres (Connection, sqlErrorMsg)
import GHC.Generics
import Query
import Schema
import Web.Spock
import Web.Spock.Config
import Web.Users.Postgresql
import Web.Users.Types (SessionId, User, UserStorageBackend, getUserById, verifySession)

-- datatypes
data IsGuest = IsGuest

-- app :: SpockM conn sess st ()
-- SpockM conn sess st = SpockCtxM () conn sess st
-- SpockCtxM ctx conn sess st = SpockCtxT ctx (WebStateM conn sess st)
type SessionVal = Maybe Web.Users.Types.SessionId

type Api ctx = SpockCtxM ctx Connection SessionVal () ()

type ApiAction ctx a = SpockActionCtx ctx Connection SessionVal () a

-- utility functions and constants
data ApiCfg
  = ApiCfg
      { acfg_db :: Text,
        acfg_db_location :: Text,
        acfg_db_port :: Integer,
        acfg_db_user :: Text,
        acfg_db_pswd :: Text,
        acfg_port :: Int,
        acfg_name :: Text
        }

-- |Parses the configuration file
parseConfig :: FilePath -> IO ApiCfg
parseConfig cfgFile = do
  cfg <- C.load [C.Required cfgFile]
  db <- C.require cfg "db"
  dbLocation <- C.require cfg "dbLocation"
  dbPort <- C.require cfg "dbPort"
  dbUser <- C.require cfg "dbUser"
  dbPassword <- C.require cfg "dbPswd"
  port <- C.require cfg "port"
  name <- C.require cfg "apiName"
  return (ApiCfg db dbLocation dbPort dbUser dbPassword port name)

-- |Function used to get the connection used to interrogate the database
getConnection :: String -> Integer -> String -> String -> String -> PoolOrConn Connection
getConnection host port user pswd name =
  PCConn
    $ ConnBuilder
        (connectWithInfo host port user pswd name)
        (closeConnection)
        (PoolCfg 1 12 1)

-- |Produces an error with the given code and description
errorJson :: Int -> Text -> ApiAction ctx ()
errorJson code message =
  json
    $ object
        [ "result" .= String "failure",
          "error" .= object ["code" .= code, "message" .= message]
          ]

baseHook :: Monad m => ActionCtxT () m (HVect '[])
baseHook = return HNil

authHook :: UserStorageBackend conn0 => ActionCtxT (HVect ts1) (WebStateM conn0 SessionVal st) (HVect (User : ts1))
authHook = do
  oldCtx <- getContext
  sess <- readSession
  mUser <- getUserFromSession
  case mUser of
    Nothing ->
      text "Unknown user. Login first!"
    Just val ->
      return (val :&: oldCtx)

getUserFromSession :: UserStorageBackend b0 => ActionCtxT ctx (WebStateM b0 SessionVal st) (Maybe User)
getUserFromSession =
  runMaybeT $ do
    sessId <- MaybeT readSession
    uid <- MaybeT $ runQuery (\conn -> verifySession conn sessId 0)
    user <- MaybeT $ runQuery (`getUserById` uid)
    return user

-- server functions
runServer :: ApiCfg -> IO ()
runServer cfg =
  let conn =
        getConnection (unpack $ acfg_db_location cfg)
          (acfg_db_port cfg)
          (unpack $ acfg_db_user cfg)
          (unpack $ acfg_db_pswd cfg)
          (unpack $ acfg_db cfg)
   in do
        spockCfg <- defaultSpockCfg Nothing conn ()
        runSpock 8080 (spock spockCfg app)

app :: Api ()
app = do
  prehook baseHook $ do
    get "" $ do
      -- sending main page with login form
      -- here goes login
      queryResult <- runQuery selectAllPeople
      case queryResult of
        Left ex -> errorJson 400 $ decodeUtf8 $ sqlErrorMsg ex
        Right allPeople -> json allPeople
    prehook authHook $ do
      -- here goes requests for pages and data
      get "materials" $ do
        queryResult <- runQuery selectAllMaterials
        case queryResult of
          Left ex -> errorJson 400 $ decodeUtf8 $ sqlErrorMsg ex
          Right allMaterials -> json allMaterials

-- orphan istances (argh) because they are not necessary for the db part of the application, only for the server one
deriving instance FromJSON Person

deriving instance ToJSON Person

deriving instance FromJSON PersonId

deriving instance ToJSON PersonId

deriving instance FromJSON (PrimaryKey PersonT (Nullable Identity))

deriving instance ToJSON (PrimaryKey PersonT (Nullable Identity))

deriving instance FromJSON Print

deriving instance ToJSON Print

deriving instance FromJSON PrintId

deriving instance ToJSON PrintId

deriving instance FromJSON Cut

deriving instance ToJSON Cut

deriving instance FromJSON CutId

deriving instance ToJSON CutId

deriving instance FromJSON Printer

deriving instance ToJSON Printer

deriving instance FromJSON PrinterId

deriving instance ToJSON PrinterId

deriving instance FromJSON (PrimaryKey PrinterT (Nullable Identity))

deriving instance ToJSON (PrimaryKey PrinterT (Nullable Identity))

deriving instance FromJSON Plastic

deriving instance ToJSON Plastic

deriving instance FromJSON PlasticId

deriving instance ToJSON PlasticId

deriving instance FromJSON Filament

deriving instance ToJSON Filament

deriving instance FromJSON FilamentId

deriving instance ToJSON FilamentId

deriving instance FromJSON Processing

deriving instance ToJSON Processing

deriving instance FromJSON ProcessingId

deriving instance ToJSON ProcessingId

deriving instance FromJSON Type

deriving instance ToJSON Type

deriving instance FromJSON TypeId

deriving instance ToJSON TypeId

deriving instance FromJSON Material

deriving instance ToJSON Material

deriving instance FromJSON MaterialId

deriving instance ToJSON MaterialId

deriving instance FromJSON MaterialsClass

deriving instance ToJSON MaterialsClass

deriving instance FromJSON MaterialsClassId

deriving instance ToJSON MaterialsClassId

deriving instance FromJSON Composition

deriving instance ToJSON Composition

deriving instance FromJSON CompositionId

deriving instance ToJSON CompositionId

deriving instance FromJSON Use

deriving instance ToJSON Use

deriving instance FromJSON UseId

deriving instance ToJSON UseId
