{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |Module used for managing users of the application
module Users where

import Control.Exception
import Data.Int (Int)
import Data.Text
import Data.Time
import Database.Beam
import Database.Beam.Backend.SQL (BeamSqlBackend)
import Database.Beam.Postgres
import Database.PostgreSQL.LibPQ (ExecStatus (NonfatalError))

-- datatypes
-- |A session id
type SessionID = Int

-- |The result of a login
data CheckUserResult
  = AllOk
  | WrongUsername
  | WrongPassword
  deriving (Eq, Show)

-- |Data representing an admin
data AdminT f
  = Admin
      { _adminUsername :: Columnar f Text,
        _adminHash :: Columnar f Text
        }
  deriving (Beamable, Generic)

instance Table AdminT where

  data PrimaryKey AdminT f = AdminId (Columnar f Text) deriving (Beamable, Generic)

  primaryKey = AdminId . _adminUsername

type Admin = AdminT Identity

type AdminId = PrimaryKey AdminT Identity

deriving instance Eq Admin

deriving instance Show Admin

deriving instance Eq AdminId

deriving instance Show AdminId

-- |Data representing a session
data SessionT f
  = Session
      { _sessionIdSessione :: Columnar f SessionID,
        _sessionOraCreazione :: Columnar f UTCTime,
        _sessionAdmin :: PrimaryKey AdminT f
        }
  deriving (Beamable, Generic)

instance Table SessionT where

  data PrimaryKey SessionT f = SessionId (Columnar f SessionID) deriving (Beamable, Generic)

  primaryKey = SessionId . _sessionIdSessione

type Session = SessionT Identity

type SessionId = PrimaryKey SessionT Identity

deriving instance Eq Session

deriving instance Show Session

deriving instance Eq SessionId

deriving instance Show SessionId

-- |Data representing the database
data AdminDB f
  = AdminDB
      { _admins :: f (TableEntity AdminT),
        _sessioni :: f (TableEntity SessionT)
        }
  deriving (Database be, Generic)

adminDb :: DatabaseSettings be AdminDB
adminDb =
  withDbModification defaultDbSettings
    dbModification
      { _sessioni =
          modifyTableFields
            tableModification
              { _sessionAdmin = AdminId (fieldNamed "admin")
                }
        }

-- functions
-- |Connects to the database with the given characteristics
connectWithInfo
  :: String -- ^ name of the host
  -> Integer -- ^ port
  -> String -- ^ username
  -> String -- ^ password
  -> String -- ^ name of the database
  -> IO Connection
connectWithInfo host port user pswd db = connect $ ConnectInfo host (fromInteger port) user pswd db

-- |Given a connection, close it
closeConnection :: Connection -> IO ()
closeConnection = close

runBeam :: Connection -> Pg a -> IO a
runBeam = runBeamPostgres --Debug putStrLn -- change for debug or production purposes

-- admins functions
-- | Insert a new admin into the database
insertAdmin :: String -> String -> (Connection -> IO (Either SqlError ()))
insertAdmin name hash =
  \conn ->
    try
      $ runBeam conn
      $ runInsert
      $ insert (_admins adminDb)
      $ insertValues
          [ Admin
              (pack name)
              (pack hash)
            ]

-- |Select the admins with the given username
selectAdminFromUsername :: String -> (Connection -> IO (Either SqlError (Maybe Admin)))
selectAdminFromUsername name =
  \conn ->
    try
      $ runBeam conn
      $ runSelectReturningOne
      $ select
      $ filter_ (\n -> _adminUsername n ==. (val_ (pack name)))
      $ all_ (_admins adminDb)

-- |Checks if a user is in the database, with the correct hash
checkUser :: String -> String -> (Connection -> IO (Either SqlError CheckUserResult))
checkUser user hash =
  \conn -> do
    mAdmin <- selectAdminFromUsername user conn
    case mAdmin of
      Left ex -> return $ Left ex
      Right Nothing -> return $ Right WrongUsername
      Right (Just admin) -> return $ Right $ if (pack hash) == (_adminHash admin) then AllOk else WrongPassword

-- sessions functions
-- | Insert a new admin into the database
insertSession :: String -> UTCTime -> (Connection -> IO (Either SqlError ()))
insertSession name time =
  \conn -> do
    selectedAdmins <- selectAdminFromUsername name conn
    case selectedAdmins of
      Left ex -> return $ Left ex
      Right Nothing -> return $ Left $ SqlError "" NonfatalError "No admin with the given username was present" "" ""
      Right (Just admin) ->
        try
          $ runBeam conn
          $ runInsert
          $ insert (_sessioni adminDb)
          $ insertExpressions
              [ Session
                  { _sessionIdSessione = default_,
                    _sessionOraCreazione = val_ time,
                    _sessionAdmin = val_ $ pk admin
                    }
                ]

-- |Select the admins with the given username
selectSessionFromId :: SessionID -> (Connection -> IO (Either SqlError (Maybe Session)))
selectSessionFromId sId =
  \conn ->
    try
      $ runBeam conn
      $ runSelectReturningOne
      $ select
      $ filter_ (\s -> _sessionIdSessione s ==. (val_ $ sId))
      $ all_
      $ _sessioni adminDb

-- |Select most recent session for a given admin
selectMostRecentSession :: String -> (Connection -> IO (Either SqlError (Maybe Session)))
selectMostRecentSession user =
  \conn -> do
    mAdmin <- selectAdminFromUsername user conn
    case mAdmin of
      Left ex -> return $ Left ex
      Right Nothing -> return $ Left $ SqlError "" NonfatalError "No admin with the given username was present" "" ""
      Right (Just admin) ->
        try
          $ runBeam conn
          $ runSelectReturningOne
          $ select
          $ limit_ 1
          $ orderBy_ (desc_ . _sessionOraCreazione)
          $ filter_ (\s -> _sessionAdmin s ==. (val_ $ pk admin))
          $ all_
          $ _sessioni adminDb

-- |Check if the session is still valid
checkSessionValidity :: Session -> IO Bool
checkSessionValidity session = do
  time <- getCurrentTime
  return $ diffUTCTime time (_sessionOraCreazione session) < (3600 * 2)
