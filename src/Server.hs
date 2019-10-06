{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.Maybe
import Data.Scientific
import Data.Text
import Data.Text.Encoding
import Data.Time
import Database.Beam
import Database.Beam.Postgres (Connection, SqlError, sqlErrorMsg)
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
-- json :: (ToJSON a, MonadIO m) => a -> ActionT m ()
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

-- |Sends a json message with the given code and description
messageJson :: MonadIO m => Int -> Text -> ActionCtxT ctx m b
messageJson code message =
  json
    $ object
        [ "response" .= object ["code" .= code, "message" .= message]
          ]

-- |Basic authentication level
baseHook :: Monad m => ActionCtxT () m (HVect '[])
baseHook = return HNil

-- |Authorized user authentication level
authHook :: ActionCtxT (HVect ts1) (WebStateM Connection SessionVal st) (HVect (User : ts1))
authHook = do
  oldCtx <- getContext
  sess <- readSession
  mUser <- getUserFromSession
  case mUser of
    Nothing -> messageJson 401 "Utente non autorizzato"
    Just val -> return (val :&: oldCtx)

-- |Admin authorization level
adminHook :: ActionCtxT (HVect ts1) (WebStateM Connection SessionVal st) (HVect (User : ts1))
adminHook = do
  oldCtx <- getContext
  sess <- readSession
  mUser <- getUserFromSession
  case mUser of
    Nothing -> redirect ""
    Just user ->
      case _userAdmin user of
        True -> return (user :&: oldCtx)
        False -> messageJson 401 "Admin non autorizzato"

-- |Function to get the user of the current session
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

-- |Functions used to log in a user
loginAction :: Text -> Text -> ApiAction ctx ()
loginAction user pswd = do
  queryResult <- runQuery $ checkUser (unpack user) (unpack pswd)
  case queryResult of
    Left ex -> messageJson 500 "Problema durante l'autenticazione"
    Right WrongUsername -> messageJson 401 "Username errata"
    Right WrongPassword -> messageJson 401 "Password errata"
    Right AllOk -> do
      time <- liftIO getCurrentTime
      insertResult <- runQuery $ insertSession (unpack user) time
      case insertResult of
        Right () -> do
          mSession <- runQuery $ selectMostRecentSession $ unpack user
          case mSession of
            Left ex -> messageJson 500 $ decodeUtf8 $ sqlErrorMsg ex
            Right Nothing -> messageJson 666 "Spero seriamente che questo testo non sarà mai mostrato"
            Right (Just session) ->
              let sid = _sessionIdSessione session
               in do
                    writeSession (Just sid)
                    redirect "app"
        Left ex -> messageJson 500 $ decodeUtf8 $ sqlErrorMsg ex

-- |Executes a query that returns a list and sends the result as a json message
executeQueryListAndSendResult
  :: (HasSpock (ActionCtxT ctx m), ToJSON a, MonadIO m)
  => (SpockConn (ActionCtxT ctx m) -> IO (Either SqlError [a]))
  -> ActionCtxT ctx m b
executeQueryListAndSendResult query = do
  queryResult <- runQuery query
  case queryResult of
    Left ex -> messageJson 500 $ decodeUtf8 $ sqlErrorMsg ex
    Right result -> json result

-- |Executes a query to insert or modify some data and sends the result as a json message
executeModifyQueryAndSendResult
  :: (HasSpock (ActionCtxT ctx m), MonadIO m)
  => (SpockConn (ActionCtxT ctx m) -> IO (Either SqlError ()))
  -> ActionCtxT ctx m b
executeModifyQueryAndSendResult query = do
  queryResult <- runQuery query
  case queryResult of
    Left ex -> messageJson 500 $ decodeUtf8 $ sqlErrorMsg ex
    Right () -> messageJson 200 "Operazione riuscita"

-- |Checks if a list of Maybe contains all Just values or not
testParameters :: [Maybe a] -> Bool
testParameters = (Prelude.all id) . (fmap isJust)

-- |Converts a String to a Bool, with "true", "True" and "TRUE" corresponding to True
toBool :: String -> Bool
toBool = flip elem ["true", "True", "TRUE"]

-- |Sends a message signalling a missing parameter
missingParameter :: MonadIO m => ActionCtxT ctx m b
missingParameter = messageJson 422 "Parametro mancante"

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
    -- routes for unauthenticated users
    get root $ do
      file "text/html" $ getClientFilePath "login.html"
    post "login" $ do
      maybeUser <- param "username"
      maybePswd <- param "password"
      if testParameters [maybeUser, maybePswd]
        then
          loginAction
            (fromJust maybeUser)
            (fromJust maybePswd)
        else missingParameter
    get "login.js"
      $ file "application/javascript"
      $ getClientFilePath "login.js"
    get ("login.css")
      $ file "text/css"
      $ getClientFilePath "login.css"
    prehook authHook $ do
      -- routes for authenticated users
      get "app" $ do
        file "text/html" $ getClientFilePath "index.html"
      post "insert_person" $ do
        maybeCf <- param "cf"
        maybeName <- param "name"
        maybeSurname <- param "surname"
        if testParameters [maybeCf, maybeName, maybeSurname]
          then
            executeModifyQueryAndSendResult
              $ insertPerson
                  (fromJust maybeCf)
                  (fromJust maybeName)
                  (fromJust maybeSurname)
          else missingParameter
      post "modify_person" $ do
        maybeCf <- param "cf"
        maybePartner <- param "partner"
        maybeCutter <- param "cutter"
        maybePrinter <- param "printer"
        if testParameters [maybeCf, maybePartner, maybeCutter, maybePrinter]
          then
            executeModifyQueryAndSendResult
              $ modifyPerson
                  (fromJust maybeCf)
                  (toBool $ fromJust maybePartner)
                  (toBool $ fromJust maybeCutter)
                  (toBool $ fromJust maybePrinter)
          else missingParameter
      post "insert_class" $ do
        maybeCode <- param "code"
        maybeName <- param "name"
        if testParameters [maybeCode, maybeName]
          then
            executeModifyQueryAndSendResult
              $ insertMaterialsClass
                  (fromJust maybeCode)
                  (fromJust maybeName)
          else missingParameter
      post "insert_material" $ do
        maybeCode <- param "code"
        maybeClass <- param "class"
        maybeName <- param "name"
        maybeWidth <- param "width"
        maybeDescr <- param "description"
        if testParameters [maybeCode, maybeClass, maybeName, maybeWidth, maybeDescr]
          then
            executeModifyQueryAndSendResult
              $ insertMaterial
                  (fromJust maybeCode)
                  (fromJust maybeClass)
                  (fromJust maybeName)
                  (read $ fromJust maybeWidth :: Double)
                  (fromJust maybeDescr)
          else missingParameter
      post "select_materials" $ do
        maybeCCode <- param "class_code"
        case maybeCCode of
          Nothing -> missingParameter
          Just cCode -> executeQueryListAndSendResult $ selectMaterialsByClass cCode
      post "insert_processing" $ do
        maybeTypeCode <- param "type"
        maybeMaterialCode <- param "material"
        maybeMaxP <- param "max_potency"
        maybeMinP <- param "min_potency"
        maybeSpeed <- param "speed"
        maybeDescr <- param "description"
        if testParameters [maybeTypeCode, maybeMaterialCode, maybeMaxP, maybeMinP, maybeSpeed, maybeDescr]
          then
            executeModifyQueryAndSendResult
              $ insertProcessing
                  (fromJust maybeTypeCode)
                  (fromJust maybeMaterialCode)
                  (read $ fromJust maybeMaxP :: Int)
                  (read $ fromJust maybeMinP :: Int)
                  (read $ fromJust maybeSpeed :: Int)
                  (fromJust maybeDescr)
          else missingParameter
      post "select_processings_by_material" $ do
        maybeMaterialCode <- param "material"
        case maybeMaterialCode of
          Nothing -> missingParameter
          Just code -> executeQueryListAndSendResult $ selectProcessingsByMaterials code
      post "insert_plastic" $ do
        maybeCode <- param "code"
        maybeName <- param "name"
        maybeDescr <- param "description"
        if testParameters [maybeCode, maybeName, maybeDescr]
          then
            executeModifyQueryAndSendResult
              $ insertPlastic
                  (fromJust maybeCode)
                  (fromJust maybeName)
                  (fromJust maybeDescr)
          else missingParameter
      post "insert_filament" $ do
        maybeCode <- param "code"
        maybePlastic <- param "plastic"
        maybeBrand <- param "brand"
        maybeColor <- param "color"
        if testParameters [maybeCode, maybePlastic, maybeBrand, maybeColor]
          then
            executeModifyQueryAndSendResult
              $ insertFilament
                  (fromJust maybeCode)
                  (fromJust maybePlastic)
                  (fromJust maybeBrand)
                  (fromJust maybeColor)
          else missingParameter
      post "select_filaments" $ do
        maybePCode <- param "plastic_code"
        case maybePCode of
          Nothing -> missingParameter
          Just code -> executeQueryListAndSendResult $ selectFilamentsByPlastic code
      post "insert_print" $ do
        maybeCf <- param "client"
        maybeDate <- param "date"
        maybeDescr <- param "descr"
        if testParameters [maybeCf, maybeDate, maybeDescr]
          then
            executeModifyQueryAndSendResult
              $ insertPrint
                  (fromJust maybeCf)
                  (read $ fromJust maybeDate :: Day)
                  (fromJust maybeDescr)
          else missingParameter
      post "assign_print_operator" $ do
        maybeCf <- param "operator"
        maybeCode <- param "print"
        if testParameters [maybeCode, maybeCf]
          then
            executeModifyQueryAndSendResult
              $ assignPrint
                  (read $ fromJust maybeCode :: Int)
                  (fromJust maybeCf)
          else missingParameter
      post "assign_print_printer" $ do
        maybeCodePrinter <- param "printer"
        maybeCode <- param "print"
        if testParameters [maybeCode, maybeCodePrinter]
          then
            executeModifyQueryAndSendResult
              $ assignPrinter
                  (fromJust maybeCodePrinter)
                  (read $ fromJust maybeCode :: Int)
          else missingParameter
      post "modify_print" $ do
        maybePrint <- param "print"
        maybeDate <- param "date"
        maybeTime <- param "time"
        maybeTotal <- param "total"
        maybeMaterials <- param "materials"
        if testParameters [maybePrint, maybeTime, maybeTotal, maybeMaterials, maybeDate]
          then
            executeModifyQueryAndSendResult
              $ completePrint
                  (read $ fromJust maybePrint :: Int)
                  (read $ fromJust maybeDate :: Day)
                  (read $ fromJust maybeTime :: Double)
                  (read $ fromJust maybeTotal :: Scientific)                  
                  (read $ fromJust maybeMaterials :: Scientific)
          else missingParameter
      post "insert_cut" $ do
        maybeCf <- param "client"
        maybeDate <- param "date"
        maybeDescr <- param "descr"
        if testParameters [maybeCf, maybeDate, maybeDescr]
          then
            executeModifyQueryAndSendResult
              $ insertCut
                  (fromJust maybeCf)
                  (read $ fromJust maybeDate :: Day)
                  (fromJust maybeDescr)
          else missingParameter
      post "assign_cut_operator" $ do
        maybeCf <- param "operator"
        maybeCode <- param "print"
        if testParameters [maybeCode, maybeCf]
          then
            executeModifyQueryAndSendResult
              $ assignCut
                  (read $ fromJust maybeCode :: Int)
                  (fromJust maybeCf)
          else missingParameter
      post "modify_cut" $  do
        maybeCut <- param "cut"
        maybeDate <- param "date"
        maybeTime <- param "time"
        maybeTotal <- param "total"
        maybeMaterials <- param "materials"
        if testParameters [maybeCut, maybeTime, maybeTotal, maybeMaterials, maybeDate]
          then
            executeModifyQueryAndSendResult
              $ completeCut
                  (read $ fromJust maybeCut :: Int)
                  (read $ fromJust maybeDate :: Day)
                  (read $ fromJust maybeTime :: Double)
                  (read $ fromJust maybeTotal :: Scientific)                  
                  (read $ fromJust maybeMaterials :: Scientific)
          else missingParameter
      get "people" $ do
        executeQueryListAndSendResult selectAllPeople
      get "partners" $ do
        executeQueryListAndSendResult selectAllPartners
      get "cutterOperators" $ do
        executeQueryListAndSendResult selectAllLaserCutterOperators
      get "printerOperators" $ do
        executeQueryListAndSendResult selectAllPrinterOperators
      get "materials" $ do
        executeQueryListAndSendResult selectAllMaterials
      get "materials_classes" $ do
        executeQueryListAndSendResult selectAllMaterialsClasses
      get "processings" $ do
        executeQueryListAndSendResult selectAllProcessings
      get "types" $ do
        executeQueryListAndSendResult selectAllTypes
      get "filaments" $ do
        executeQueryListAndSendResult selectAllFilaments
      get "plastics" $ do
        executeQueryListAndSendResult selectAllPlastics
      get "printers" $ do
        executeQueryListAndSendResult selectAllPrinters
      get "prints" $ do
        executeQueryListAndSendResult selectAllPrints
      get "incomplete_prints" $ do
        executeQueryListAndSendResult selectAllIncompletePrints
      get "complete_prints" $ do
        executeQueryListAndSendResult selectAllCompletePrints
      get "cuts" $ do
        executeQueryListAndSendResult selectAllCuts
      get "incomplete_cuts" $ do
        executeQueryListAndSendResult selectAllIncompleteCuts
      get "complete_cuts" $ do
        executeQueryListAndSendResult selectAllCompleteCuts
      get "index.js"
        $ file "application/javascript"
        $ getClientFilePath "index.js"
      get ("index.css")
        $ file "text/css"
        $ getClientFilePath "index.css"
      prehook adminHook $ do
        -- routes for authenticated admins
        get "manager" $ do
          text "with great power comes great responsability!"
        post "insert_type" $ do
          maybeCode <- param "code"
          maybeName <- param "name"
          maybeDescr <- param "description"
          if testParameters [maybeCode, maybeName, maybeDescr]
            then
              executeModifyQueryAndSendResult
                $ insertType
                    (fromJust maybeCode)
                    (fromJust maybeName)
                    (fromJust maybeDescr)
            else missingParameter
        post "insert_printer" $ do
          maybeCode <- param "code"
          maybeBrand <- param "brand"
          maybeModel <- param "model"
          maybeDescr <- param "description"
          if testParameters [maybeCode, maybeBrand, maybeModel, maybeDescr]
            then
              executeModifyQueryAndSendResult
                $ insertPrinter
                    (fromJust maybeCode)
                    (fromJust maybeBrand)
                    (fromJust maybeModel)
                    (fromJust maybeDescr)
            else missingParameter
        post "insert_user" $ do
          maybeUser <- param "username"
          maybePswd <- param "password"
          if testParameters [maybeUser, maybePswd]
            then
              executeModifyQueryAndSendResult
                $ insertUser
                    (fromJust maybeUser)
                    (fromJust maybePswd)
            else messageJson 401 "Parametro mancante"

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
