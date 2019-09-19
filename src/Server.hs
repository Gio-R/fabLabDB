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
import Data.Aeson hiding (json)
import qualified Data.Configurator as C
import Data.HVect
import Data.List as L (groupBy)
import Data.Text
import Data.Text.Encoding
import Data.Time
import Database.Beam
import Database.Beam.Postgres (Connection, sqlErrorMsg)
import GHC.Generics
import Network.Wai.Middleware.Static
import Query as Q
import Schema
import Users as U
import Web.Spock
import Web.Spock.Config

-- datatypes
data IsGuest = IsGuest

-- |Represents the information necessary to start the application
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

-- app :: SpockM conn sess st ()
-- SpockM conn sess st = SpockCtxM () conn sess st
-- SpockCtxM ctx conn sess st = SpockCtxT ctx (WebStateM conn sess st)
type SessionVal = Maybe SessionID

type Api ctx = SpockCtxM ctx Connection SessionVal () ()

type ApiAction ctx a = SpockActionCtx ctx Connection SessionVal () a

-- utility functions and constants
getClientFilePath :: String -> FilePath
getClientFilePath fileName = "client/" ++ fileName

getFileName :: String -> String
getFileName partialUri = Prelude.last $ L.groupBy (\x y -> y /= '/') partialUri

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
getFabLabConnection
  :: String -- ^ Host name
  -> Integer -- ^ Port
  -> String -- ^ Username
  -> String -- ^ Password
  -> String -- ^ Name of the database
  -> IO Connection
getFabLabConnection = Q.connectWithInfo

-- |Function used to get the PoolOrConn necessary to interrogate the database
getPoolOrConn :: IO Connection -> PoolOrConn Connection
getPoolOrConn conn =
  PCConn
    $ ConnBuilder
        conn
        (Q.closeConnection)
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

authHook :: ActionCtxT (HVect ts1) (WebStateM Connection SessionVal st) (HVect (User : ts1))
authHook = do
  oldCtx <- getContext
  sess <- readSession
  mUser <- getUserFromSession
  case mUser of
    Nothing -> redirect ""
    Just val -> return (val :&: oldCtx)

getUserFromSession :: ActionCtxT ctx (WebStateM Connection SessionVal st) (Maybe User)
getUserFromSession =
  do
    sessId <- readSession
    case sessId of
      Nothing -> return Nothing
      Just sId -> do
        queryResult <- runQuery $ selectSessionFromId $ sId
        case queryResult of
          Left ex -> return Nothing
          Right Nothing -> return Nothing
          Right (Just session) ->
            let (UserId id) = _sessionUtente session
            in do
                  queryResult' <- runQuery $ selectUserFromUsername $ unpack id
                  case queryResult' of
                    Left ex -> return Nothing
                    Right a -> return a

loginAction :: Text -> Text -> ApiAction ctx ()
loginAction user pswd = do
      queryResult <- runQuery $ checkUser (unpack user) (unpack pswd)
      case queryResult of
        Left ex -> text "There was a problem during your authentication"
        Right WrongUsername -> text "Wrong username"
        Right WrongPassword -> text "Wrong password"
        Right AllOk -> do
          time <- liftIO getCurrentTime
          insertResult <- runQuery $ insertSession (unpack user) time
          case insertResult of
            Right () -> do
              mSession <- runQuery $ selectMostRecentSession $ unpack user
              case mSession of
                Left ex -> text $ decodeUtf8 $ sqlErrorMsg ex
                Right Nothing -> text "I seriously hope this text will never be displayed"
                Right (Just session) ->
                  let sid = _sessionIdSessione session
                    in do
                        writeSession (Just sid)
                        redirect "app"
            Left ex -> text $ decodeUtf8 $ sqlErrorMsg ex

-- server functions
runServer :: ApiCfg -> IO ()
runServer cfg =
  let ioConn =
        getFabLabConnection (unpack $ acfg_db_location cfg)
          (acfg_db_port cfg)
          (unpack $ acfg_db_user cfg)
          (unpack $ acfg_db_pswd cfg)
          (unpack $ acfg_db cfg)
   in do
        spockCfg <- defaultSpockCfg Nothing (getPoolOrConn ioConn) ()
        runSpock 8080 (spock spockCfg app)

app :: Api ()
app = do
  middleware $ staticPolicy $ addBase "static"
  prehook baseHook $ do
    get root $ do
      file "text/html" $ getClientFilePath "login.html"
    post "login" $ do
      maybeUser <- param "username"
      maybePswd <- param "password"
      case (maybeUser, maybePswd) of
        (Just user, Just pswd) -> loginAction user pswd
        (_, _) -> errorJson 400 "Missing parameter"
    get "index.js" $
      file "application/javascript" $ getClientFilePath "index.js"
    get ("index.css") $ 
      file "text/css" $ getClientFilePath "index.css"
    get "login.js" $
      file "application/javascript" $ getClientFilePath "login.js"
    get ("login.css") $ 
      file "text/css" $ getClientFilePath "login.css"
    prehook authHook $ do
      -- here goes requests for pages and data
      get "app" $ do
        file "text/html" $ getClientFilePath "index.html"
      get "people" $ do
        queryResult <- runQuery selectAllPeople
        case queryResult of
          Left ex -> errorJson 400 $ decodeUtf8 $ sqlErrorMsg ex
          Right allPeople -> json allPeople

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
