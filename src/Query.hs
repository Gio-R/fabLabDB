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

import Control.Exception
import Data.ByteString.UTF8 (fromString, toString)
import Data.Int (Int)
import Data.Scientific
import Data.Text
import Data.Time.Calendar
import Database.Beam
import Database.Beam.Backend.SQL (BeamSqlBackend)
import Database.Beam.Postgres
import Schema

-- constants and general functions
-- |Creates an uri for connecting to the database with the given username and password
createUri :: String -> String -> String
createUri user pswd = "postgres://" ++ user ++ ":" ++ pswd ++ "@localhost/FabLab"

-- |Given an uri, returns a connection to the database
connect :: String -> IO Connection
connect uri = connectPostgreSQL $ fromString uri

-- |Given a connection, close it
closeConnection :: Connection -> IO ()
closeConnection = close

runBeam :: Connection -> Pg a -> IO a
runBeam = runBeamPostgres --Debug putStrLn -- change for debug or production purposes

allElementsOfTable
  :: (Table t, BeamSqlBackend be)
  => ( DatabaseSettings be FabLabDB
       -> DatabaseEntity be FabLabDB (TableEntity t)
       )
  -> Q be FabLabDB s (t (QExpr be s))
allElementsOfTable table = all_ (table fabLabDB)

-- |A generic select with filters
{-genericSelect :: (Table t, Generic (t Identity),
                        Generic (t Database.Beam.Backend.Types.Exposed),
                        Database.Beam.Backend.SQL.Row.GFromBackendRow
                          Postgres
                          (GHC.Generics.Rep (t Database.Beam.Backend.Types.Exposed))
                          (GHC.Generics.Rep (t Identity))) =>
                       (DatabaseSettings Postgres FabLabDB
                        -> DatabaseEntity Postgres FabLabDB (TableEntity t))
                       -> Maybe
                            (t (QExpr Postgres QBaseScope) -> QExpr Postgres QBaseScope Bool)
                       -> Connection
                       -> IO (Either SomeException [t Identity])-}
genericSelect table maybeFilter =
  let pool = case maybeFilter of
        Nothing -> allElementsOfTable table
        Just f -> filter_ f $ allElementsOfTable table
   in \conn ->
        try
          $ runBeam conn
          $ runSelectReturningList
          $ select pool

-- |Prepares a code to be used as key
prepareCode :: String -> Text
prepareCode = toUpper . pack

-- |Prepares a string to be used as a name in the database
prepareName :: String -> Text
prepareName = toTitle . pack

-- people queries
-- |Select all people in the database
selectAllPeople :: Connection -> IO (Either SqlError [Person])
selectAllPeople = genericSelect _persone Nothing

-- |Select all laser cutter operators in the database
selectAllLaserCutterOperators :: Connection -> IO (Either SqlError [Person])
selectAllLaserCutterOperators =
  genericSelect _persone $ Just (\p -> _personOperatoreIntagliatrice p ==. (val_ True))

-- |Select all 3D printer operators in the database
selectAllPrinterOperators :: Connection -> IO (Either SqlError [Person])
selectAllPrinterOperators =
  genericSelect _persone $ Just (\p -> _personOperatoreStampante p ==. (val_ True))

-- |Select all people with the given cf (should be 0 or 1) in the database
selectPersonFromCF :: String -> (Connection -> IO (Either SqlError [Person]))
selectPersonFromCF cf =
  genericSelect _persone $ Just (\p -> _personCf p ==. (val_ (pack cf)))

-- |Add a person to the database
insertPerson :: String -> String -> String -> (Connection -> IO (Either SqlError ()))
insertPerson cf name surname =
  \conn ->
    try
      $ runBeam conn
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
modifyPerson :: String -> Bool -> Bool -> Bool -> (Connection -> IO (Either SqlError ()))
modifyPerson cf partner cutter printer =
  \conn ->
    try
      $ runBeam conn
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
selectAllMaterials :: Connection -> IO (Either SqlError [Material])
selectAllMaterials = genericSelect _materiali Nothing

-- |Select all classes of materials in the database
selectAllMaterialsClasses :: Connection -> IO (Either SqlError [MaterialsClass])
selectAllMaterialsClasses = genericSelect _classi_di_materiali Nothing

-- |Select all the materials classes with the given code (should be 1 or 0) in the database
selectMaterialsClassFromCode :: String -> (Connection -> IO (Either SqlError [MaterialsClass]))
selectMaterialsClassFromCode code =
  genericSelect _classi_di_materiali $ Just (\c -> _materialsclassCodiceClasse c ==. (val_ (prepareCode code)))

-- |Select all materials with the given code (should be 1 or 0) in the database
selectMaterialFromCode :: String -> (Connection -> IO (Either SqlError [Material]))
selectMaterialFromCode code =
  genericSelect _materiali $ Just (\m -> _materialCodiceMateriale m ==. (val_ (prepareCode code)))

-- |Select all materials of a given class in the database
selectMaterialsByClass :: String -> (Connection -> IO (Either SqlError [Material]))
selectMaterialsByClass classCode =
  \conn -> do
    selectedClasses <- (selectMaterialsClassFromCode classCode) conn
    case selectedClasses of
      Left ex -> pure $ Left ex
      Right classes ->
        let mClass = Prelude.head classes
         in try $ runBeam conn
              $ runSelectReturningList
              $ select
              $ filter_ (\m -> _materialCodiceClasse m ==. val_ (pk mClass))
              $ allElementsOfTable _materiali

-- |Add a class of materials to the database in the database
insertMaterialsClass :: String -> String -> (Connection -> IO (Either SqlError ()))
insertMaterialsClass code name =
  \conn ->
    try
      $ runBeam conn
      $ runInsert
      $ insert (_classi_di_materiali fabLabDB)
      $ insertValues
          [ MaterialsClass
              (prepareCode code)
              (prepareName name)
            ]

-- |Add a material to the database. The code is the id of the material inside the materials class.
insertMaterial :: String -> String -> String -> Double -> String -> (Connection -> IO (Either SqlError ()))
insertMaterial code classCode name width descr =
  \conn -> do
    selectedClasses <- (selectMaterialsClassFromCode classCode) conn
    case selectedClasses of
      Left ex -> pure $ Left ex
      Right classes ->
        let mClass = Prelude.head classes
         in try $ runBeam conn
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
-- |Select the processing with the given code in the database
selectProcessingFromCode :: String -> (Connection -> IO (Either SqlError [Processing]))
selectProcessingFromCode pCode =
  genericSelect _lavorazioni $ Just (\p -> _processingCodiceLavorazione p ==. val_ (prepareCode pCode))

-- |Select all processings in the database
selectAllProcessings :: Connection -> IO (Either SqlError [Processing])
selectAllProcessings = genericSelect _lavorazioni Nothing

-- |Select all types of processing in the database
selectAllTypes :: Connection -> IO (Either SqlError [Type])
selectAllTypes = genericSelect _tipi Nothing

-- |Select all types of processing with the given code (should be 1 or 0) in the database
selectTypeFromCode :: String -> (Connection -> IO (Either SqlError [Type]))
selectTypeFromCode code =
  genericSelect _tipi $ Just (\t -> _typeCodiceTipo t ==. (val_ (prepareCode code)))

-- |Select all processings on a given material in the database
selectProcessingsByMaterials :: String -> (Connection -> IO (Either SqlError [Processing]))
selectProcessingsByMaterials mCode =
  \conn -> do
    selectedMaterials <- (selectMaterialFromCode mCode) conn
    case selectedMaterials of
      Left ex -> pure $ Left ex
      Right materials ->
        let material = Prelude.head materials
         in try $ runBeam conn
              $ runSelectReturningList
              $ select
              $ filter_ (\p -> _processingCodiceMateriale p ==. val_ (pk material))
              $ allElementsOfTable _lavorazioni

-- |Add a type of processing to the database
insertType :: String -> String -> String -> (Connection -> IO (Either SqlError ()))
insertType code name descr =
  \conn ->
    try
      $ runBeam conn
      $ runInsert
      $ insert (_tipi fabLabDB)
      $ insertValues
          [ Type
              (prepareCode code)
              (prepareName name)
              (pack descr)
            ]

-- |Add a new processing to the database
insertProcessing :: String -> String -> Int -> Int -> Int -> String -> (Connection -> IO (Either SqlError ()))
insertProcessing typeCode materialCode maxPotency minPotency speed descr =
  \conn -> do
    selectedTypes <- (selectTypeFromCode typeCode) conn
    selectedMaterials <- (selectMaterialFromCode materialCode) conn
    case (selectedTypes, selectedMaterials) of
      (Left ex, Left ex') -> pure $ Left $ (error $ (toString $ sqlErrorMsg ex) ++ (toString $ sqlErrorMsg ex'))
      (Left ex, _) -> pure $ Left ex
      (_, Left ex) -> pure $ Left ex
      (Right types, Right materials) ->
        let pType = Prelude.head types :: Type
            material = Prelude.head materials :: Material
            code = materialCode ++ (show maxPotency) ++ (show minPotency) ++ (show speed) ++ typeCode
         in try $ runBeam conn
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
selectAllFilaments :: Connection -> IO (Either SqlError [Filament])
selectAllFilaments = genericSelect _filamenti Nothing

-- |Select all plastics in the database
selectAllPlastics :: Connection -> IO (Either SqlError [Plastic])
selectAllPlastics = genericSelect _plastiche Nothing

-- |Select all the plastics with the given code (should be 1 or 0) in the database
selectPlasticFromCode :: String -> (Connection -> IO (Either SqlError [Plastic]))
selectPlasticFromCode code =
  genericSelect _plastiche $ Just (\p -> _plasticCodicePlastica p ==. (val_ (prepareCode code)))

-- |Select all the filaments with the given code (should be 1 or 0) in the database
selectFilamentFromCode :: String -> (Connection -> IO (Either SqlError [Filament]))
selectFilamentFromCode code =
  genericSelect _filamenti $ Just (\f -> _filamentCodiceFilamento f ==. (val_ (prepareCode code)))

-- |Select the filaments made of a given plastic in the database
selectFilamentsByPlastic :: String -> (Connection -> IO (Either SqlError [Filament]))
selectFilamentsByPlastic plasticCode =
  \conn -> do
    selectedPlastics <- (selectPlasticFromCode plasticCode) conn
    case selectedPlastics of
      Left ex -> pure $ Left ex
      Right plastics ->
        let plastic = Prelude.head plastics :: Plastic
         in try $ runBeam conn
              $ runSelectReturningList
              $ select
              $ filter_ (\f -> _filamentCodicePlastica f ==. val_ (pk plastic))
              $ allElementsOfTable _filamenti

-- |Add a type of plastic to the database
insertPlastic :: String -> String -> String -> (Connection -> IO (Either SqlError ()))
insertPlastic code name descr =
  \conn ->
    try
      $ runBeam conn
      $ runInsert
      $ insert (_plastiche fabLabDB)
      $ insertValues
          [ Plastic
              (prepareCode code)
              (prepareName name)
              (pack descr)
            ]

-- |Add a filament to the database. The code is the id of the filament inside the type of plastic
insertFilament :: String -> String -> String -> String -> (Connection -> IO (Either SqlError ()))
insertFilament code plasticCode brand color =
  \conn -> do
    selectedPlastics <- (selectPlasticFromCode plasticCode) conn
    case selectedPlastics of
      Left ex -> pure $ Left ex
      Right plastics ->
        let plastic = Prelude.head plastics
         in try $ runBeam conn
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
selectAllPrinters :: Connection -> IO (Either SqlError [Printer])
selectAllPrinters = genericSelect _stampanti Nothing

-- |Select all the printers with the given code (should be 0 or 1) in the database
selectPrinterFromCode :: String -> (Connection -> IO (Either SqlError [Printer]))
selectPrinterFromCode code =
  genericSelect _stampanti $ Just (\p -> _printerCodiceStampante p ==. (val_ (prepareCode code)))

-- |Add a printer to the database
insertPrinter :: String -> String -> String -> String -> (Connection -> IO (Either SqlError ()))
insertPrinter code brand model descr =
  \conn ->
    try
      $ runBeam conn
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
assignPrinter :: String -> Int -> (Connection -> IO (Either SqlError ()))
assignPrinter printerCode printCode =
  \conn -> do
    selectedPrinters <- (selectPrinterFromCode printerCode) conn
    case selectedPrinters of
      Left ex -> pure $ Left ex
      Right printers ->
        let printer = Prelude.head printers
         in try $ runBeam conn
              $ runUpdate
              $ update (_stampe fabLabDB)
                  (\s -> _printCodiceStampante s <-. just_ (val_ (pk printer)))
                  (\s -> _printCodiceStampa s ==. (val_ printCode))

-- prints queries
-- |Select the print with the given code in the database (should be 0 or 1)
selectPrintFromCode :: Int -> (Connection -> IO (Either SqlError [Print]))
selectPrintFromCode pCode =
  genericSelect _stampe $ Just (\p -> _printCodiceStampa p ==. val_ pCode)

-- |Select all prints in the database
selectAllPrints :: Connection -> IO (Either SqlError [Print])
selectAllPrints = genericSelect _stampe Nothing

-- |Select all the print that aren't completed in the database
selectAllIncompletePrints :: Connection -> IO (Either SqlError [Print])
selectAllIncompletePrints =
  genericSelect _stampe $ Just (\p -> _printDataConsegna p ==. val_ Nothing)

-- |Select all the completed prints in the database
selectAllCompletePrints :: Connection -> IO (Either SqlError [Print])
selectAllCompletePrints =
  genericSelect _stampe $ Just (\p -> _printDataConsegna p /=. val_ Nothing)

-- |Add a new print to the database
insertPrint :: String -> Day -> String -> (Connection -> IO (Either SqlError ()))
insertPrint cf insertDate descr =
  \conn -> do
    selectedPeople <- (selectPersonFromCF cf) conn
    case selectedPeople of
      Left ex -> pure $ Left ex
      Right people ->
        let person = Prelude.head people
         in try $ runBeam conn
              $ runInsert
              $ insert (_stampe fabLabDB)
              $ insertExpressions
                  [ Print
                      { _printCodiceStampa = default_,
                        _printDataRichiesta = val_ insertDate,
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
assignPrint :: Int -> String -> (Connection -> IO (Either SqlError ()))
assignPrint code cf =
  \conn -> do
    selectedOperators <- (selectPersonFromCF cf) conn
    case selectedOperators of
      Left ex -> pure $ Left ex
      Right operators ->
        let operator = Prelude.head operators
         in try $ runBeam conn
              $ runUpdate
              $ update (_stampe fabLabDB)
                  (\s -> _printCfIncaricato s <-. just_ (val_ (pk operator)))
                  (\s -> _printCodiceStampa s ==. val_ code)

-- |Assign a filament to a print in the database
assignFilament :: Int -> String -> (Connection -> IO (Either SqlError ()))
assignFilament pCode fCode =
  \conn -> do
    selectedFilaments <- (selectFilamentFromCode fCode) conn
    selectedPrints <- (selectPrintFromCode pCode) conn
    case (selectedFilaments, selectedPrints) of
      (Left ex, Left ex') -> pure $ Left $ (error $ (toString $ sqlErrorMsg ex) ++ (toString $ sqlErrorMsg ex'))
      (Left ex, _) -> pure $ Left ex
      (_, Left ex) -> pure $ Left ex
      (Right filaments, Right prints) ->
        let filament = Prelude.head filaments
            selectedPrint = Prelude.head prints
         in try $ runBeam conn
              $ runInsert
              $ insert (_usi fabLabDB)
              $ insertExpressions
                  [ Use
                      { _useCodiceFilamento = val_ (pk filament),
                        _useCodiceStampa = val_ (pk selectedPrint)
                        }
                    ]

-- |Complete a print
completePrint :: Int -> Day -> Double -> Scientific -> Scientific -> (Connection -> IO (Either SqlError ()))
completePrint pCode deliveryDate workTime total materials =
  \conn ->
    try
      $ runBeam conn
      $ runUpdate
      $ update (_stampe fabLabDB)
          ( \s ->
              mconcat
                [ _printDataConsegna s <-. val_ (Just deliveryDate),
                  _printCostoMateriali s <-. val_ (Just materials),
                  _printCostoTotale s <-. val_ (Just total),
                  _printTempo s <-. val_ (Just workTime)
                  ]
            )
          (\s -> _printCodiceStampa s ==. val_ pCode)

-- cuts queries
-- |Select the cut with the given code in the database (should be 0 or 1)
selectCutFromCode :: Int -> (Connection -> IO (Either SqlError [Cut]))
selectCutFromCode cCode =
  genericSelect _intagli $ Just (\c -> _cutCodiceIntaglio c ==. val_ cCode)

-- |Select all cuts in the database
selectAllCuts :: Connection -> IO (Either SqlError [Cut])
selectAllCuts = genericSelect _intagli Nothing

-- |Select all the print that aren't completed in the database
selectAllIncompleteCuts :: Connection -> IO (Either SqlError [Cut])
selectAllIncompleteCuts =
  genericSelect _intagli $ Just (\c -> _cutDataConsegna c ==. val_ Nothing)

-- |Select all the completed prints in the database
selectAllCompleteCuts :: Connection -> IO (Either SqlError [Cut])
selectAllCompleteCuts =
  genericSelect _intagli $ Just (\c -> _cutDataConsegna c /=. val_ Nothing)

-- |Add a new cut to the database
insertCut :: String -> Day -> String -> (Connection -> IO (Either SqlError ()))
insertCut cf insertDate descr =
  \conn -> do
    selectedPeople <- selectPersonFromCF cf conn
    case selectedPeople of
      Left ex -> pure $ Left ex
      Right people ->
        let person = Prelude.head people
         in try $ runBeam conn
              $ runInsert
              $ insert (_intagli fabLabDB)
              $ insertExpressions
                  [ Cut
                      { _cutCodiceIntaglio = default_,
                        _cutDataRichiesta = val_ insertDate,
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
assignCut :: Int -> String -> (Connection -> IO (Either SqlError ()))
assignCut code cf =
  \conn -> do
    selectedOperators <- (selectPersonFromCF cf) conn
    case selectedOperators of
      Left ex -> pure $ Left ex
      Right operators ->
        let operator = Prelude.head operators
         in try $ runBeam conn
              $ runUpdate
              $ update (_intagli fabLabDB)
                  (\c -> _cutCfIncaricato c <-. just_ (val_ (pk operator)))
                  (\c -> _cutCodiceIntaglio c ==. val_ code)

-- |Assign a processing to a print
assignProcessing :: Int -> String -> (Connection -> IO (Either SqlError ()))
assignProcessing cCode pCode =
  \conn -> do
    selectedProcessings <- (selectProcessingFromCode pCode) conn
    selectedCuts <- (selectCutFromCode cCode) conn
    case (selectedProcessings, selectedCuts) of
      (Left ex, Left ex') -> pure $ Left $ (error $ (toString $ sqlErrorMsg ex) ++ (toString $ sqlErrorMsg ex'))
      (Left ex, _) -> pure $ Left ex
      (_, Left ex) -> pure $ Left ex
      (Right processings, Right cuts) -> do
        let processing = Prelude.head processings
            cut = Prelude.head cuts
         in try $ runBeam conn
              $ runInsert
              $ insert (_composizioni fabLabDB)
              $ insertExpressions
                  [ Composition
                      { _compositionCodiceLavorazione = val_ (pk processing),
                        _compositionCodiceIntaglio = val_ (pk cut)
                        }
                    ]

-- |Complete a cut
completeCut :: Int -> Day -> Double -> Scientific -> Scientific -> (Connection -> IO (Either SqlError ()))
completeCut code deliveryDate workTime total materials =
  \conn ->
    try
      $ runBeam conn
      $ runUpdate
      $ update (_intagli fabLabDB)
          ( \c ->
              mconcat
                [ _cutDataConsegna c <-. val_ (Just deliveryDate),
                  _cutCostoTotale c <-. val_ (Just total),
                  _cutCostoMateriali c <-. val_ (Just materials),
                  _cutTempo c <-. val_ (Just workTime)
                  ]
            )
          (\c -> _cutCodiceIntaglio c ==. val_ code)
