-- |Module used for connecting to the database
{-# LANGUAGE OverloadedStrings #-}
module Database where

import Data.Text
import Data.Int (Int64)
import Data.ByteString (ByteString)
import Data.Scientific
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow

-- costants
uri :: ByteString
uri = "postgres://postgres:14102002@localhost/FabLab"

newPersonString :: Query
newPersonString = "INSERT INTO persone (cf, nome, cognome, socio, operatore_intagliatrice, operatore_stampante, spesa_totale) \
                 \ values (?, ?, ?, ?, ?, ?, ?)"

changePersonStatusString :: Query
changePersonStatusString = "UPDATE persone SET socio = ?, operatore_intagliatrice = ?, operatore_stampante = ? WHERE cf = ?"

increasePersonExpenseString :: Query
increasePersonExpenseString = "UPDATE persone SET spesa_totale = spesa_totale + ? where cf = ?"

-- datatypes
-- |Data representing a person in the database
data Person = Person { cf :: Text
                     , nome :: Text
                     , cognome :: Text
                     , socio :: Bool
                     , operatoreIntagliatrice :: Bool
                     , operatoreStampante :: Bool
                     , spesaTotale :: Scientific
                     } deriving (Show)
instance Eq Person where
    (==) p p' = (cf p) == (cf p')
instance FromRow Person where
    fromRow = Person <$> field <*> field <*> field <*> field <*> field <*> field <*> field
instance ToRow Person where
    toRow p = [ toField (cf p)
              , toField (nome p)
              , toField (cognome p)
              , toField (socio p)
              , toField (operatoreIntagliatrice p)
              , toField (operatoreStampante p)
              , toField (spesaTotale p)
              ]

-- miscellaneus functions
validateLength :: Int -> String -> Maybe String
validateLength n s = if Prelude.length s == n then Just s else Nothing

mkPerson :: String -> String -> String -> Maybe Person
mkPerson cf nome cognome = Person <$> (pack <$> validateLength 16 cf) 
                                  <*> (pack <$> validateLength 30 nome) 
                                  <*> (pack <$> validateLength 30 cognome)
                                  <*> Just False
                                  <*> Just False
                                  <*> Just False
                                  <*> Just 0

changePersonStatus :: Person -> Bool -> Bool -> Bool -> Person
changePersonStatus p socio opIntagliatrice opStampante = Person (cf p) (nome p) (cognome p) socio opIntagliatrice opStampante (spesaTotale p)

-- people queries
-- |Function to add a person to the database using the given connection
addPerson :: Connection -> Person -> IO Int64
addPerson conn person = execute conn newPersonString person

-- |Function to modify a person status in the database, updating it to the one of the given Person, using the given connection
modifyPersonStatus :: Connection -> Person -> IO Int64
modifyPersonStatus conn updatedPerson = 
    execute conn changePersonStatusString ( socio updatedPerson
                                          , operatoreIntagliatrice updatedPerson
                                          , operatoreStampante updatedPerson
                                          , cf updatedPerson)

-- |Function to increase the total expense of the given person, using the given connection
increasePersonExpense :: Connection -> Person -> Double -> IO Int64
increasePersonExpense conn person increment = execute conn increasePersonExpenseString (increment, (cf person))