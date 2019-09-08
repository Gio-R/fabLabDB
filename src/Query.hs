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
import Database.Beam.Backend.SQL (BeamSqlBackend)
import Database.Beam.Postgres
import Database.Beam.Query
import Schema

-- constants and general functions
-- |Creates an uri for connecting to the database with the given username and password
createUri :: String -> String -> String
createUri user pswd = "postgres://" ++ user ++ ":" ++ pswd ++ "@localhost/FabLab"

-- |Given an uri, returns a connection to the database
connect :: String -> IO Connection
connect uri = connectPostgreSQL $ fromString uri

runBeam :: Connection -> Pg a -> IO a
runBeam = runBeamPostgresDebug putStrLn -- change for debug or production purposes

allElementsOfTable
  :: (Table t, BeamSqlBackend be)
  => ( DatabaseSettings be FabLabDB
       -> DatabaseEntity be FabLabDB (TableEntity t)
       )
  -> Q be FabLabDB s (t (QExpr be s))
allElementsOfTable table = all_ (table fabLabDB)

-- |A generic select with filters
-- | table :: Table t => ( DatabaseSettings be FabLabDB -> DatabaseEntity be FabLabDB (TableEntity t))
-- | filter :: Table t => (t (QExpr be s)) -> QExpr be s Bool
selectWithFilters table filter = 
  \conn ->
    runBeam conn
    $ runSelectReturningList
    $ select
    $ filter_ filter $ allElementsOfTable table

-- |Prepares a code to be used as key
prepareCode :: String -> Text
prepareCode = toUpper . pack

-- |Prepares a string to be used as a name in the database
prepareName :: String -> Text
prepareName = toTitle . pack

-- people queries
-- |Select all people in the database
selectAllPeople :: Connection -> IO [Person]
selectAllPeople conn =
  runBeam conn
    $ runSelectReturningList
    $ select $ allElementsOfTable _persone

-- |Select all laser cutter operators in the database
selectAllLaserCutterOperators :: Connection -> IO [Person]
selectAllLaserCutterOperators conn =
  runBeam conn
    $ runSelectReturningList
    $ select
    $ filter_ (\p -> _personOperatoreIntagliatrice p ==. (val_ True)) $ allElementsOfTable _persone

-- |Select all 3D printer operators in the database
selectAllPrinterOperators :: Connection -> IO [Person]
selectAllPrinterOperators conn =
  runBeam conn
    $ runSelectReturningList
    $ select
    $ filter_ (\p -> _personOperatoreStampante p ==. (val_ True)) $ allElementsOfTable _persone

-- |Select all people with the given cf (should be 0 or 1) in the database
selectPersonFromCF :: String -> (Connection -> IO [Person])
selectPersonFromCF cf =
  \conn ->
    runBeam conn
      $ runSelectReturningList
      $ select
      $ filter_ (\p -> _personCf p ==. (val_ (pack cf))) $ allElementsOfTable _persone

-- |Add a person to the database
insertPerson :: String -> String -> String -> (Connection -> IO ())
insertPerson cf name surname =
  \conn ->
    runBeam conn
      $ runInsert
      $ insert (_persone fabLabDB)
      $ insertValues
          [ Person
              (prepareCode cf)
              (prepareName name)
              (prepareName surname)
              False
              False
              False
              0.0
            ]

-- |Modify a person already in the database
modifyPerson :: String -> Bool -> Bool -> Bool -> (Connection -> IO ())
modifyPerson cf partner cutter printer =
  \conn ->
    runBeam conn
      $ runUpdate
      $ update (_persone fabLabDB)
          ( \p ->
              mconcat
                [ _personSocio p <-. (val_ partner),
                  _personOperatoreIntagliatrice p <-. (val_ cutter),
                  _personOperatoreStampante p <-. (val_ printer)
                  ]
            )
          (\p -> _personCf p ==. (val_ (prepareCode cf)))

-- materials queries
-- |Select all materials in the database
selectAllMaterials :: Connection -> IO [Material]
selectAllMaterials =
  \conn ->
    runBeam conn
      $ runSelectReturningList
      $ select $ allElementsOfTable _materiali

-- |Select all classes of materials in the database
selectAllMaterialsClasses :: Connection -> IO [MaterialsClass]
selectAllMaterialsClasses =
  \conn ->
    runBeam conn
      $ runSelectReturningList
      $ select $ allElementsOfTable _classi_di_materiali

-- |Select all the materials classes with the given code (should be 1 or 0) in the database
selectMaterialsClassFromCode :: String -> (Connection -> IO [MaterialsClass])
selectMaterialsClassFromCode code =
  \conn ->
    runBeam conn
      $ runSelectReturningList
      $ select
      $ filter_ (\c -> _materialsclassCodiceClasse c ==. (val_ (prepareCode code))) $ allElementsOfTable _classi_di_materiali

-- |Select all materials with the given code (should be 1 or 0) in the database
selectMaterialFromCode :: String -> (Connection -> IO [Material])
selectMaterialFromCode code =
  \conn ->
    runBeam conn
      $ runSelectReturningList
      $ select
      $ filter_ (\m -> _materialCodiceMateriale m ==. (val_ (prepareCode code))) $ allElementsOfTable _materiali

-- |Select all materials of a given class in the database
selectMaterialsByClass :: String -> (Connection -> IO [Material])
selectMaterialsByClass classCode =
  \conn -> do
    classes <- (selectMaterialsClassFromCode classCode) conn
    let mClass = Prelude.head classes :: MaterialsClass
     in runBeam conn
          $ runSelectReturningList
          $ select
          $ filter_ (\m -> _materialCodiceClasse m ==. val_ (pk mClass)) $ allElementsOfTable _materiali

-- |Add a class of materials to the database in the database
insertMaterialsClass :: String -> String -> (Connection -> IO ())
insertMaterialsClass code name =
  \conn ->
    runBeam conn
      $ runInsert
      $ insert (_classi_di_materiali fabLabDB)
      $ insertValues
          [ MaterialsClass
              (prepareCode code)
              (prepareName name)
            ]

-- |Add a material to the database. The code is the id of the material inside the materials class.
insertMaterial :: String -> String -> String -> Double -> String -> (Connection -> IO ())
insertMaterial code classCode name width descr =
  \conn -> do
    classes <- (selectMaterialsClassFromCode classCode) conn
    let mClass = Prelude.head classes :: MaterialsClass
     in runBeam conn
          $ runInsert
          $ insert (_materiali fabLabDB)
          $ insertValues
              [ Material
                  (pk mClass)
                  (prepareCode (classCode ++ code))
                  (prepareName name)
                  width
                  (pack descr)
                ]

-- processings queries
-- |Select all processings in the database
selectAllProcessings :: Connection -> IO [Processing]
selectAllProcessings =
  \conn ->
    runBeam conn
      $ runSelectReturningList
      $ select $ allElementsOfTable _lavorazioni

-- |Select all types of processing in the database
selectAllTypes :: Connection -> IO [Type]
selectAllTypes =
  \conn ->
    runBeam conn
      $ runSelectReturningList
      $ select $ allElementsOfTable _tipi

-- |Select all types of processing with the given code (should be 1 or 0) in the database
selectTypeFromCode :: String -> (Connection -> IO [Type])
selectTypeFromCode code =
  \conn ->
    let
     in runBeam conn
          $ runSelectReturningList
          $ select
          $ filter_ (\t -> _typeCodiceTipo t ==. (val_ (prepareCode code))) $ allElementsOfTable _tipi

-- |Select all processings on a given material in the database
selectProcessingsByMaterials :: String -> (Connection -> IO [Processing])
selectProcessingsByMaterials mCode =
  \conn -> do
    materials <- (selectMaterialFromCode mCode) conn
    let material = Prelude.head materials :: Material
     in runBeam conn
          $ runSelectReturningList
          $ select
          $ filter_ (\p -> _processingCodiceMateriale p ==. val_ (pk material)) $ allElementsOfTable _lavorazioni

-- |Add a type of processing to the database
insertType :: String -> String -> String -> (Connection -> IO ())
insertType code name descr =
  \conn ->
    runBeam conn
      $ runInsert
      $ insert (_tipi fabLabDB)
      $ insertValues
          [ Type
              (prepareCode code)
              (prepareName name)
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
     in runBeam conn
          $ runInsert
          $ insert (_lavorazioni fabLabDB)
          $ insertValues
              [ Processing
                  (pk pType)
                  (prepareCode code)
                  (pk material)
                  maxPotency
                  minPotency
                  speed
                  (pack descr)
                ]

-- plastics and filaments queries
-- |Select all filaments in the database
selectAllFilaments :: Connection -> IO [Filament]
selectAllFilaments =
  \conn ->
    runBeam conn
      $ runSelectReturningList
      $ select $ allElementsOfTable _filamenti

-- |Select all plastics in the database
selectAllPlastics :: Connection -> IO [Plastic]
selectAllPlastics =
  \conn ->
    runBeam conn
      $ runSelectReturningList
      $ select $ allElementsOfTable _plastiche

-- |Select all the plastics with the given code (should be 1 or 0) in the database
selectPlasticFromCode :: String -> (Connection -> IO [Plastic])
selectPlasticFromCode code =
  \conn ->
    runBeam conn
      $ runSelectReturningList
      $ select
      $ filter_ (\p -> _plasticCodicePlastica p ==. (val_ (prepareCode code))) $ allElementsOfTable _plastiche

-- |Select the filaments made of a given plastic in the database
selectFilamentsByPlastic :: String -> (Connection -> IO [Filament])
selectFilamentsByPlastic pCode =
  \conn -> do
    plastics <- (selectPlasticFromCode pCode) conn
    let plastic = Prelude.head plastics :: Plastic
     in runBeam conn
          $ runSelectReturningList
          $ select
          $ filter_ (\f -> _filamentCodicePlastica f ==. val_ (pk plastic)) $ allElementsOfTable _filamenti

-- |Add a type of plastic to the database
insertPlastic :: String -> String -> String -> (Connection -> IO ())
insertPlastic code name descr =
  \conn ->
    runBeam conn
      $ runInsert
      $ insert (_plastiche fabLabDB)
      $ insertValues
          [ Plastic
              (prepareCode code)
              (prepareName name)
              (pack descr)
            ]

-- |Add a filament to the database. The code is the id of the filament inside the type of plastic
insertFilament :: String -> String -> String -> String -> (Connection -> IO ())
insertFilament code plasticCode brand color =
  \conn -> do
    plastics <- (selectPlasticFromCode plasticCode) conn
    let plastic = Prelude.head plastics :: Plastic
     in runBeam conn
          $ runInsert
          $ insert (_filamenti fabLabDB)
          $ insertValues
              [ Filament
                  (prepareCode code)
                  (pk plastic)
                  (prepareName brand)
                  (prepareName color)
                ]

-- printers queries
-- |Select all printers in the database
selectAllPrinters :: Connection -> IO [Printer]
selectAllPrinters =
  \conn ->
    runBeam conn
      $ runSelectReturningList
      $ select $ allElementsOfTable _stampanti

-- |Select all the printers with the given code (should be 0 or 1) in the database
selectPrinterFromCode :: String -> (Connection -> IO [Printer])
selectPrinterFromCode code =
  \conn ->
    runBeam conn
      $ runSelectReturningList
      $ select
      $ filter_ (\p -> _printerCodiceStampante p ==. (val_ (prepareCode code))) $ allElementsOfTable _stampanti

-- |Add a printer to the database
insertPrinter :: String -> String -> String -> String -> (Connection -> IO ())
insertPrinter code brand model descr =
  \conn -> do
    runBeam conn
      $ runInsert
      $ insert (_stampanti fabLabDB)
      $ insertValues
          [ Printer
              (prepareCode code)
              (prepareName brand)
              (prepareName model)
              (pack descr)
            ]

-- |Assign a printer to a print
assignPrinter :: String -> Int -> (Connection -> IO ())
assignPrinter printerCode print =
  \conn -> do
    printers <- (selectPrinterFromCode printerCode) conn
    let printer = Prelude.head printers
     in runBeam conn
          $ runUpdate
          $ update (_stampe fabLabDB)
              (\s -> _printCodiceStampante s <-. just_ (val_ (pk printer)))
              (\s -> _printCodiceStampa s ==. (val_ print))

-- prints queries
-- |Select all prints in the database
selectAllPrints :: Connection -> IO [Print]
selectAllPrints =
  \conn ->
    runBeam conn
      $ runSelectReturningList
      $ select $ allElementsOfTable _stampe

-- |Select all the print that aren't completed in the database
selectAllIncompletePrints :: Connection -> IO [Print]
selectAllIncompletePrints =
  \conn ->
    runBeam conn
      $ runSelectReturningList
      $ select
      $ filter_ (\p -> _printDataConsegna p ==. val_ Nothing) $ allElementsOfTable _stampe

-- |Select all the completed prints in the database
selectAllCompletePrints :: Connection -> IO [Print]
selectAllCompletePrints =
  \conn ->
    runBeam conn
      $ runSelectReturningList
      $ select
      $ filter_ (\p -> _printDataConsegna p /=. val_ Nothing) $ allElementsOfTable _stampe

-- |Add a new print to the database
insertPrint :: String -> Day -> String -> (Connection -> IO ())
insertPrint cf date descr =
  \conn -> do
    people <- (selectPersonFromCF cf) conn
    let person = Prelude.head people :: Person
     in runBeam conn
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
assignPrint code cf =
  \conn -> do
    operators <- (selectPersonFromCF cf) conn
    let operator = Prelude.head operators
     in runBeam conn
          $ runUpdate
          $ update (_stampe fabLabDB)
              (\s -> _printCfIncaricato s <-. just_ (val_ (pk operator)))
              (\s -> _printCodiceStampa s ==. val_ code)

-- |Complete a print
completePrint :: Int -> Day -> Double -> Scientific -> Scientific -> (Connection -> IO ())
completePrint print date time total materials =
  \conn ->
    runBeam conn
      $ runUpdate
      $ update (_stampe fabLabDB)
          ( \s ->
              mconcat
                [ _printDataConsegna s <-. val_ (Just date),
                  _printCostoMateriali s <-. val_ (Just materials),
                  _printCostoTotale s <-. val_ (Just total),
                  _printTempo s <-. val_ (Just time)
                  ]
            )
          (\s -> _printCodiceStampa s ==. val_ print)

-- cuts queries
-- |Select all cuts in the database
selectAllCuts :: Connection -> IO [Cut]
selectAllCuts =
  \conn ->
    runBeam conn
      $ runSelectReturningList
      $ select $ allElementsOfTable _intagli

-- |Select all the print that aren't completed in the database
selectAllIncompleteCuts :: Connection -> IO [Cut]
selectAllIncompleteCuts =
  \conn ->
    runBeam conn
      $ runSelectReturningList
      $ select
      $ filter_ (\c -> _cutDataConsegna c ==. val_ Nothing) $ allElementsOfTable _intagli

-- |Select all the completed prints in the database
selectAllCompleteCuts :: Connection -> IO [Cut]
selectAllCompleteCuts =
  \conn ->
    runBeam conn
      $ runSelectReturningList
      $ select
      $ filter_ (\c -> _cutDataConsegna c /=. val_ Nothing) $ allElementsOfTable _intagli

-- |Add a new cut to the database
insertCut :: String -> Day -> String -> (Connection -> IO ())
insertCut cf date descr =
  \conn -> do
    people <- selectPersonFromCF cf conn
    let person = Prelude.head people :: Person
     in runBeam conn
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
assignCut code cf =
  \conn -> do
    operators <- (selectPersonFromCF cf) conn
    let operator = Prelude.head operators
     in runBeam conn
          $ runUpdate
          $ update (_intagli fabLabDB)
              (\c -> _cutCfIncaricato c <-. just_ (val_ (pk operator)))
              (\c -> _cutCodiceIntaglio c ==. val_ code)

-- |Complete a cut
completeCut :: Int -> Day -> Double -> Scientific -> Scientific -> (Connection -> IO ())
completeCut code date time total materials =
  \conn ->
    runBeam conn
      $ runUpdate
      $ update (_intagli fabLabDB)
          ( \c ->
              mconcat
                [ _cutDataConsegna c <-. val_ (Just date),
                  _cutCostoTotale c <-. val_ (Just total),
                  _cutCostoMateriali c <-. val_ (Just materials),
                  _cutTempo c <-. val_ (Just time)
                  ]
            )
          (\c -> _cutCodiceIntaglio c ==. val_ code)
