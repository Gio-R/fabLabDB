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
import Crypto.KDF.BCrypt (hashPassword, validatePassword)
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 as BSU
import Data.Int (Int)
import Data.Text as T
import Data.Text.Encoding
import Data.Time
import Database.Beam
import Database.Beam.Backend.SQL (BeamSqlBackend)
import Database.Beam.Postgres
import Database.PostgreSQL.LibPQ (ExecStatus (NonfatalError))

-- datatypes
-- |A session id
type SessionID = Int

-- |The result of a user login
data CheckUserResult
  = AllOk
  | WrongUsername
  | WrongPassword
  deriving (Eq, Show)

-- |The result of an admin login
data CheckAdminResult
  = AdminOk
  | WrongLogin
  | NotAnAdmin
  deriving (Eq, Show)

-- |Data representing an user
data UserT f
  = User
      { _userUsername :: Columnar f Text,
        _userHash :: Columnar f BS.ByteString,
        _userAdmin :: Columnar f Bool
        }
  deriving (Beamable, Generic)

instance Table UserT where

  data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Beamable, Generic)

  primaryKey = UserId . _userUsername

type User = UserT Identity

type UserId = PrimaryKey UserT Identity

deriving instance Eq User

deriving instance Show User

deriving instance Eq UserId

deriving instance Show UserId

-- |Data representing a session
data SessionT f
  = Session
      { _sessionIdSessione :: Columnar f SessionID,
        _sessionOraCreazione :: Columnar f UTCTime,
        _sessionUtente :: PrimaryKey UserT f
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
data UserDB f
  = UserDB
      { _utenti :: f (TableEntity UserT),
        _sessioni :: f (TableEntity SessionT)
        }
  deriving (Database be, Generic)

userDb :: DatabaseSettings be UserDB
userDb =
  withDbModification defaultDbSettings
    dbModification
      { _sessioni =
          modifyTableFields
            tableModification
              { _sessionUtente = UserId (fieldNamed "utente")
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

-- users functions
-- | Insert a new user into the database
insertUser :: String -> String -> (Connection -> IO (Either SqlError ()))
insertUser name pswd =
  \conn -> do
    hash <- hashPassword 12 $ BSU.fromString pswd
    try
      $ runBeam conn
      $ runInsert
      $ insert (_utenti userDb)
      $ insertValues
          [ User
              (T.pack name)
              hash
              False
            ]

-- |Select the users with the given username
selectUserFromUsername :: String -> (Connection -> IO (Either SqlError (Maybe User)))
selectUserFromUsername name =
  \conn ->
    try
      $ runBeam conn
      $ runSelectReturningOne
      $ select
      $ filter_ (\n -> _userUsername n ==. (val_ (T.pack name)))
      $ all_ (_utenti userDb)

-- |Checks if a user is in the database, with the correct password
checkUser :: String -> String -> (Connection -> IO (Either SqlError CheckUserResult))
checkUser user pswd =
  \conn -> do
    mUser <- selectUserFromUsername user conn
    case mUser of
      Left ex -> return $ Left ex
      Right Nothing -> return $ Right WrongUsername
      Right (Just user) -> return $ Right $ if validatePassword (BSU.fromString pswd) (_userHash user) then AllOk else WrongPassword

-- |Checks if a user is an admin in the database, with the correct password
checkAdmin :: String -> String -> (Connection -> IO (Either SqlError CheckAdminResult))
checkAdmin user pswd =
  \conn -> do
    checkResult <- checkUser user pswd conn
    case checkResult of
      Left ex -> return $ Left ex
      Right AllOk -> do
        mUser <- selectUserFromUsername user conn
        case mUser of
          Left ex -> return $ Left ex
          Right Nothing -> return $ Right WrongLogin
          Right (Just user) -> return $ Right $ if _userAdmin user then AdminOk else NotAnAdmin
      _ -> return $ Right $ WrongLogin

-- sessions functions
-- | Insert a new user into the database
insertSession :: String -> UTCTime -> (Connection -> IO (Either SqlError ()))
insertSession name time =
  \conn -> do
    selectedUsers <- selectUserFromUsername name conn
    case selectedUsers of
      Left ex -> return $ Left ex
      Right Nothing -> return $ Left $ SqlError "" NonfatalError "No user with the given username was present" "" ""
      Right (Just user) ->
        try
          $ runBeam conn
          $ runInsert
          $ insert (_sessioni userDb)
          $ insertExpressions
              [ Session
                  { _sessionIdSessione = default_,
                    _sessionOraCreazione = val_ time,
                    _sessionUtente = val_ $ pk user
                    }
                ]

-- |Select the users with the given username
selectSessionFromId :: SessionID -> (Connection -> IO (Either SqlError (Maybe Session)))
selectSessionFromId sId =
  \conn ->
    try
      $ runBeam conn
      $ runSelectReturningOne
      $ select
      $ filter_ (\s -> _sessionIdSessione s ==. (val_ $ sId))
      $ all_
      $ _sessioni userDb

-- |Select most recent session for a given user
selectMostRecentSession :: String -> (Connection -> IO (Either SqlError (Maybe Session)))
selectMostRecentSession user =
  \conn -> do
    mUser <- selectUserFromUsername user conn
    case mUser of
      Left ex -> return $ Left ex
      Right Nothing -> return $ Left $ SqlError "" NonfatalError "No user with the given username was present" "" ""
      Right (Just user) ->
        try
          $ runBeam conn
          $ runSelectReturningOne
          $ select
          $ limit_ 1
          $ orderBy_ (desc_ . _sessionOraCreazione)
          $ filter_ (\s -> _sessionUtente s ==. (val_ $ pk user))
          $ all_
          $ _sessioni userDb

-- |Check if the session is still valid
checkSessionValidity :: Session -> IO Bool
checkSessionValidity session = do
  time <- getCurrentTime
  return $ diffUTCTime time (_sessionOraCreazione session) < (3600 * 2)
