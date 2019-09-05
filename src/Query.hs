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

-- |Module used for the queries in the database
module Query where

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Data.Int (Int)
import Data.Scientific
import Data.Text
import Data.Time.Calendar
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Query
import Schema

-- constants
runBeam :: (String -> IO ()) -> Connection -> Pg a -> IO a
runBeam = runBeamPostgresDebug -- change for debug or production purposes

allPeople :: Q Postgres FabLabDB s (PersonT (QExpr Postgres s))
allPeople = all_ (_persone fabLabDB)

allMaterialsClasses :: Q Postgres FabLabDB s (MaterialsClassT (QExpr Postgres s))
allMaterialsClasses = all_ (_classi_di_materiali fabLabDB)

allMaterials :: Q Postgres FabLabDB s (MaterialT (QExpr Postgres s))
allMaterials = all_ (_materiali fabLabDB)

allTypes :: Q Postgres FabLabDB s (TypeT (QExpr Postgres s))
allTypes = all_ (_tipi fabLabDB)

allPlastics :: Q Postgres FabLabDB s (PlasticT (QExpr Postgres s))
allPlastics = all_ (_plastiche fabLabDB)

-- |Given an uri, returns a connection to the database
connect :: String -> IO Connection
connect uri = connectPostgreSQL $ fromString uri

-- people queries
-- |Select all people in the database
selectAllPeople :: Connection -> IO [Person]
selectAllPeople conn =
  runBeam putStrLn conn
    $ runSelectReturningList
    $ select allPeople

-- |Select all laser cutter operators in the database
selectLaserCutterOperators :: Connection -> IO [Person]
selectLaserCutterOperators conn =
  runBeam putStrLn conn
    $ runSelectReturningList
    $ select
    $ filter_ (\p -> _personOperatoreIntagliatrice p ==. (val_ True)) allPeople

-- |Select all 3D printer operators in the database
selectPrinterOperators :: Connection -> IO [Person]
selectPrinterOperators conn =
  runBeam putStrLn conn
    $ runSelectReturningList
    $ select
    $ filter_ (\p -> _personOperatoreStampante p ==. (val_ True)) allPeople

-- |Select all people with the given cf (should be 0 or 1)
selectPersonFromCF :: String -> (Connection -> IO [Person])
selectPersonFromCF cf =
  \conn ->
    runBeam putStrLn conn
      $ runSelectReturningList
      $ select
      $ filter_ (\p -> _personCf p ==. (val_ (pack cf))) allPeople

-- |Add a person to the database
insertPerson :: String -> String -> String -> (Connection -> IO ())
insertPerson cf name surname =
  \conn ->
    runBeam putStrLn conn
      $ runInsert
      $ insert (_persone fabLabDB)
      $ insertValues
          [ Person
              (pack cf)
              (pack name)
              (pack surname)
              False
              False
              False
              0.0
            ]

-- |Modify a person already in the database
modifyPerson :: String -> Bool -> Bool -> Bool -> (Connection -> IO ())
modifyPerson cf partner cutter printer =
  \conn ->
    runBeam putStrLn conn
      $ runUpdate
      $ update (_persone fabLabDB)
          ( \p ->
              mconcat
                [ _personSocio p <-. (val_ partner),
                  _personOperatoreIntagliatrice p <-. (val_ cutter),
                  _personOperatoreStampante p <-. (val_ printer)
                  ]
            )
          (\p -> _personCf p ==. (val_ (pack cf)))

-- materials queries
-- |Select all the materials classes with the given code (should be 1 or 0)
selectMaterialsClassFromCode :: String -> (Connection -> IO [MaterialsClass])
selectMaterialsClassFromCode code =
  \conn ->
    runBeam putStrLn conn
      $ runSelectReturningList
      $ select
      $ filter_ (\c -> _materialsclassCodiceClasse c ==. (val_ (pack code))) allMaterialsClasses

-- |Select all materials with the given code (should be 1 or 0)
selectMaterialFromCode :: String -> (Connection -> IO [Material])
selectMaterialFromCode code =
  \conn ->
    runBeam putStrLn conn
      $ runSelectReturningList
      $ select
      $ filter_ (\m -> _materialCodiceMateriale m ==. (val_ (pack code))) allMaterials

-- |Select all types of processing with the given code (should be 1 or 0)
selectTypeFromCode :: String -> (Connection -> IO [Type])
selectTypeFromCode code =
  \conn ->
    runBeam putStrLn conn
      $ runSelectReturningList
      $ select
      $ filter_ (\t -> _typeCodiceTipo t ==. (val_ (pack code))) allTypes

-- |Add a class of materials to the database
insertMaterialsClass :: String -> String -> (Connection -> IO ())
insertMaterialsClass code name =
  \conn ->
    runBeam putStrLn conn
      $ runInsert
      $ insert (_classi_di_materiali fabLabDB)
      $ insertValues
          [ MaterialsClass
              (pack code)
              (pack name)
            ]

-- |Add a material to the database. The code is the id of the material inside the materials class.
insertMaterial :: String -> String -> String -> Double -> String -> (Connection -> IO ())
insertMaterial code classCode name width descr =
  \conn -> do
    classes <- (selectMaterialsClassFromCode classCode) conn
    let mClass = Prelude.head classes :: MaterialsClass
     in runBeam putStrLn conn
          $ runInsert
          $ insert (_materiali fabLabDB)
          $ insertValues
              [ Material
                  (pk mClass)
                  (pack (classCode ++ code))
                  (pack name)
                  width
                  (pack descr)
                ]

-- processings queries
-- |Add a type of processing to the database
insertType :: String -> String -> String -> (Connection -> IO ())
insertType code name descr =
  \conn ->
    runBeam putStrLn conn
      $ runInsert
      $ insert (_tipi fabLabDB)
      $ insertValues
          [ Type
              (pack code)
              (pack name)
              (pack descr)
            ]

-- |Add a new processing to the database
insertProcessing :: String -> String -> Int -> Int -> Int -> String -> (Connection -> IO ())
insertProcessing typeCode materialCode maxPotency minPotency speed descr =
  \conn -> do
    types <- (selectTypeFromCode typeCode) conn
    materials <- (selectMaterialFromCode materialCode) conn
    let pType = Prelude.head types :: Type
        material = Prelude.head materials :: Material
        code = materialCode ++ (show maxPotency) ++ (show minPotency) ++ (show speed) ++ typeCode
     in runBeam putStrLn conn
          $ runInsert
          $ insert (_lavorazioni fabLabDB)
          $ insertValues
              [ Processing
                  (pk pType)
                  (pack code)
                  (pk material)
                  maxPotency
                  minPotency
                  speed
                  (pack descr)
                ]

-- plastics and filaments queries
-- |Select all the plastics with the given code (should be 1 or 0)
selectPlasticFromCode :: String -> (Connection -> IO [Plastic])
selectPlasticFromCode code =
  \conn ->
    runBeam putStrLn conn
      $ runSelectReturningList
      $ select
      $ filter_ (\p -> _plasticCodicePlastica p ==. (val_ (pack code))) allPlastics

-- |Add a type of plastic to the database
insertPlastic :: String -> String -> String -> (Connection -> IO ())
insertPlastic code name descr = undefined

-- |Add a filament to the database. The code is the id of the filament inside the type of plastic
insertFilament :: String -> String -> String -> String -> (Connection -> IO ())
insertFilament code plasticCode brand color =
  \conn -> do
    plastics <- (selectPlasticFromCode plasticCode) conn
    let plastic = Prelude.head plastics :: Plastic
     in runBeam putStrLn conn
          $ runInsert
          $ insert (_filamenti fabLabDB)
          $ insertValues
              [ Filament
                  (pack code)
                  (pk plastic)
                  (pack brand)
                  (pack color)
                ]

-- printers queries
-- |Add a printer to the database
insertPrinter :: String -> String -> String -> String -> (Connection -> IO ())
insertPrinter code brand model descr =
  \conn -> do
    runBeam putStrLn conn
      $ runInsert
      $ insert (_stampanti fabLabDB)
      $ insertValues [Printer (pack code) (pack brand) (pack model) (pack descr)]

-- |Assign a printer to a print
assignPrinter :: String -> Int -> (Connection -> IO ())
assignPrinter printer print = undefined

-- prints queries
-- |Add a new print to the database
insertPrint :: String -> Day -> String -> (Connection -> IO ())
insertPrint cf date descr =
  \conn -> do
    people <- (selectPersonFromCF cf) conn
    let person = Prelude.head people :: Person
     in runBeam putStrLn conn
          $ runInsert
          $ insert (_stampe fabLabDB)
          $ insertExpressions
              [ Print
                  { _printCodiceStampa = default_,
                    _printDataRichiesta = val_ date,
                    _printDataConsegna = val_ Nothing,
                    _printTempo = val_ Nothing,
                    _printCostoMateriali = val_ Nothing,
                    _printCostoTotale = val_ Nothing,
                    _printDescrizione = val_ (pack descr),
                    _printCfRichiedente = val_ (pk person),
                    _printCfIncaricato = nothing_,
                    _printCodiceStampante = nothing_
                    }
                ]

-- |Assign a print to an operator
assignPrint :: Int -> String -> (Connection -> IO ())
assignPrint code cf = undefined

-- |Complete a print
completePrint :: Day -> Double -> Scientific -> Scientific -> (Connection -> IO ())
completePrint date time total materials = undefined

-- cuts queries
-- |Add a new cut to the database
insertCut :: String -> Day -> String -> (Connection -> IO ())
insertCut cf date descr =
  \conn -> do
    people <- selectPersonFromCF cf conn
    let person = Prelude.head people :: Person
     in runBeam putStrLn conn
          $ runInsert
          $ insert (_intagli fabLabDB)
          $ insertExpressions
              [ Cut
                  { _cutCodiceIntaglio = default_,
                    _cutDataRichiesta = val_ date,
                    _cutDataConsegna = val_ Nothing,
                    _cutTempo = val_ Nothing,
                    _cutCostoMateriali = val_ Nothing,
                    _cutCostoTotale = val_ Nothing,
                    _cutDescrizione = val_ (pack descr),
                    _cutCfRichiedente = val_ (pk person),
                    _cutCfIncaricato = nothing_
                    }
                ]

-- |Assign a cut to an operator
assignCut :: Int -> String -> (Connection -> IO ())
assignCut code cf = undefined

-- |Complete a cut
completeCut :: Day -> Double -> Scientific -> Scientific -> (Connection -> IO ())
completeCut date time total materials = undefined
