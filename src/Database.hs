-- |Module used for connecting to the database
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
module Database where

import Data.Text
import Data.Int (Int64)
import Data.ByteString (ByteString)
import Data.Scientific
import Data.Time.Calendar
import Database.Beam
import Database.Beam.Postgres

-- costants
uri :: ByteString
uri = "postgres://postgres:14102002@localhost/FabLab"

runBeam :: (String -> IO ()) -> Connection -> Pg a -> IO a
runBeam = runBeamPostgresDebug -- change for debug or production purposes

-- datatypes
-- |Data representing a person in the database
data PersonT f = Person { _personCf :: Columnar f Text
                        , _personNome :: Columnar f Text
                        , _personCognome :: Columnar f Text
                        , _personSocio :: Columnar f Bool
                        , _personOperatoreIntagliatrice :: Columnar f Bool
                        , _personOperatoreStampante :: Columnar f Bool
                        , _personSpesaTotale :: Columnar f Scientific
                        } deriving (Beamable, Generic)
instance Table PersonT where
    data PrimaryKey PersonT f = PersonId (Columnar f Text) deriving (Beamable, Generic)
    primaryKey = PersonId . _personCf
type Person = PersonT Identity
type PersonId = PrimaryKey PersonT Identity
deriving instance Eq Person
deriving instance Show Person
deriving instance Eq PersonId
deriving instance Show PersonId
deriving instance Eq (PrimaryKey PersonT (Nullable Identity))
deriving instance Show (PrimaryKey PersonT (Nullable Identity))

-- |Data representing a print
data PrintT f = Print { _printCodiceStampa :: Columnar f Integer
                      , _printDataRichiesta :: Columnar f Day
                      , _printDataConsegna :: Columnar f (Maybe Day)
                      , _printTempo :: Columnar f (Maybe Double)
                      , _printCostoMateriali :: Columnar f (Maybe Scientific)
                      , _printCostoTotale :: Columnar f (Maybe Scientific)
                      , _printDescrizione :: Columnar f Text
                      , _printCfRichiedente :: PrimaryKey PersonT f
                      , _printCfIncaricato :: PrimaryKey PersonT (Nullable f)
                      , _printCodiceStampante :: PrimaryKey PrinterT (Nullable f)
                      } deriving (Beamable, Generic)
instance Table PrintT where
    data PrimaryKey PrintT f = PrintId (Columnar f Integer) deriving (Beamable, Generic)
    primaryKey = PrintId . _printCodiceStampa
type Print = PrintT Identity
type PrintId = PrimaryKey PrintT Identity
deriving instance Eq Print
deriving instance Show Print
deriving instance Eq PrintId
deriving instance Show PrintId

-- |Data representing a cut
data CutT f = Cut { _cutCodiceIntaglio :: Columnar f Integer
                  , _cutDataRichiesta :: Columnar f Day
                  , _cutDataConsegna :: Columnar f (Maybe Day)
                  , _cutTempo :: Columnar f (Maybe Double)
                  , _cutCostoMateriali :: Columnar f (Maybe Scientific)
                  , _cutCostoTotale :: Columnar f (Maybe Scientific)
                  , _cutDescrizione :: Columnar f Text
                  , _cutCfRichiedente :: PrimaryKey PersonT f
                  , _cutCfIncaricato :: PrimaryKey PersonT (Nullable f)
                  } deriving (Beamable, Generic)
instance Table CutT where
    data PrimaryKey CutT f = CutId (Columnar f Integer) deriving (Beamable, Generic)
    primaryKey = CutId . _cutCodiceIntaglio
type Cut = CutT Identity
type CutId = PrimaryKey CutT Identity
deriving instance Eq Cut
deriving instance Show Cut
deriving instance Eq CutId
deriving instance Show CutId

-- |Data representing a printer
data PrinterT f = Printer { _printerCodiceStampante :: Columnar f Text
                          , _printerMarca :: Columnar f Text
                          , _printerModello :: Columnar f Text
                          , _printerDescrizione :: Columnar f Text
                          } deriving (Beamable, Generic)
instance Table PrinterT where
    data PrimaryKey PrinterT f = PrinterId (Columnar f Text) deriving (Beamable, Generic)
    primaryKey = PrinterId . _printerCodiceStampante
type Printer = PrinterT Identity
type PrinterId = PrimaryKey PrinterT Identity
deriving instance Eq Printer
deriving instance Show Printer
deriving instance Eq PrinterId
deriving instance Show PrinterId
deriving instance Eq (PrimaryKey PrinterT (Nullable Identity))
deriving instance Show (PrimaryKey PrinterT (Nullable Identity))

-- |Data representing a type of plastic
data PlasticT f = Plastic { _plasticCodicePlastica :: Columnar f Text
                          , _plasticNome :: Columnar f Text
                          , _plasticDescrizione :: Columnar f Text
                          } deriving (Beamable, Generic)
instance Table PlasticT where
    data PrimaryKey PlasticT f = PlasticId (Columnar f Text) deriving (Beamable, Generic)
    primaryKey = PlasticId . _plasticCodicePlastica
type Plastic = PlasticT Identity
type PlasticId = PrimaryKey PlasticT Identity
deriving instance Eq Plastic
deriving instance Show Plastic
deriving instance Eq PlasticId
deriving instance Show PlasticId

-- |Data representing a filament
data FilamentT f = Filament { _filamentCodiceFilamento :: Columnar f Text
                            , _filamentCodicePlastica :: PrimaryKey PlasticT f
                            , _filamentMarca :: Columnar f Text
                            , _filamentColore :: Columnar f Text
                            } deriving (Beamable, Generic)
instance Table FilamentT where
    data PrimaryKey FilamentT f = FilamentId (Columnar f Text) deriving (Beamable, Generic)
    primaryKey = FilamentId . _filamentCodiceFilamento
type Filament = FilamentT Identity
type FilamentId = PrimaryKey FilamentT Identity
deriving instance Eq Filament
deriving instance Show Filament
deriving instance Eq FilamentId
deriving instance Show FilamentId

-- |Data representing a way of executing a cut
data ProcessingT f = Processing { _processingCodiceTipo :: PrimaryKey TypeT f
                                , _processingCodiceLavorazione :: Columnar f Text
                                , _processingCodiceMateriale :: PrimaryKey MaterialT f
                                , _processingPotenzaMassima :: Columnar f Text
                                , _processingPotenzaMinima :: Columnar f Text
                                , _processingVelocita :: Columnar f Text
                                , _processingDescrizione :: Columnar f Text
                                } deriving (Beamable, Generic)
instance Table ProcessingT where
    data PrimaryKey ProcessingT f = ProcessingId (Columnar f Text) deriving (Beamable, Generic)
    primaryKey = ProcessingId . _processingCodiceLavorazione
type Processing = ProcessingT Identity
type ProcessingId = PrimaryKey ProcessingT Identity
deriving instance Eq Processing
deriving instance Show Processing
deriving instance Eq ProcessingId
deriving instance Show ProcessingId

-- |Data representing a type of processing
data TypeT f = Type { _typeCodiceTipo :: Columnar f Text 
                    , _typeNome :: Columnar f Text
                    , _typeDescrizione :: Columnar f Text
                    } deriving (Beamable, Generic)
instance Table TypeT where
    data PrimaryKey TypeT f = TypeId (Columnar f Text) deriving (Beamable, Generic)
    primaryKey = TypeId . _typeCodiceTipo
type Type = TypeT Identity
type TypeId = PrimaryKey TypeT Identity
deriving instance Eq Type
deriving instance Show Type
deriving instance Eq TypeId
deriving instance Show TypeId

-- |Data representing a material
data MaterialT f = Material { _materialCodiceClasse :: PrimaryKey MaterialsClassT f
                            , _materialCodiceMateriale :: Columnar f Text
                            , _materialNome :: Columnar f Text
                            , _materialSpessore :: Columnar f Double
                            , _materialDescrizione :: Columnar f Text
                            } deriving (Beamable, Generic)
instance Table MaterialT where
    data PrimaryKey MaterialT f = MaterialId (Columnar f Text) deriving (Beamable, Generic)
    primaryKey = MaterialId . _materialCodiceMateriale
type Material = MaterialT Identity
type MaterialId = PrimaryKey MaterialT Identity
deriving instance Eq Material
deriving instance Show Material
deriving instance Eq MaterialId
deriving instance Show MaterialId

-- |Data representing a class of materials
data MaterialsClassT f = MaterialsClass { _materialsclassCodiceClasse :: Columnar f Text
                                        , _materialsclassNome :: Columnar f Text
                                        } deriving (Beamable, Generic)
instance Table MaterialsClassT where
    data PrimaryKey MaterialsClassT f = MaterialsClassId (Columnar f Text) deriving (Beamable, Generic)
    primaryKey = MaterialsClassId . _materialsclassCodiceClasse
type MaterialsClass = MaterialsClassT Identity
type MaterialsClassId = PrimaryKey MaterialsClassT Identity
deriving instance Eq MaterialsClass
deriving instance Show MaterialsClass
deriving instance Eq MaterialsClassId
deriving instance Show MaterialsClassId

-- |Data representing a processing used in a cut
data CompositionT f = Composition { _compositionCodiceLavorazione :: PrimaryKey ProcessingT f
                                  , _compositionCodiceIntaglio :: PrimaryKey CutT f
                                  } deriving (Beamable, Generic)
instance Table CompositionT where
    data PrimaryKey CompositionT f = CompositionId (PrimaryKey ProcessingT f) (PrimaryKey CutT f) deriving (Beamable, Generic)
    primaryKey = CompositionId <$> _compositionCodiceLavorazione <*> _compositionCodiceIntaglio
type Composition = CompositionT Identity
type CompositionId = PrimaryKey CompositionT Identity
deriving instance Eq Composition
deriving instance Show Composition
deriving instance Eq CompositionId
deriving instance Show CompositionId

-- |Data representing a filament used in a print
data UseT f = Use { _useCodiceFilamento :: PrimaryKey FilamentT f
                  , _useCodiceStampa :: PrimaryKey PrintT f
                  } deriving (Beamable, Generic)
instance Table UseT where
    data PrimaryKey UseT f = UseId (PrimaryKey FilamentT f) (PrimaryKey PrintT f) deriving (Beamable, Generic)
    primaryKey = UseId <$> _useCodiceFilamento <*> _useCodiceStampa
type Use = UseT Identity
type UseId = PrimaryKey UseT Identity
deriving instance Eq Use
deriving instance Show Use
deriving instance Eq UseId
deriving instance Show UseId

-- |Data representing the database
data FabLabDB f = FabLabDB { _persone :: f (TableEntity PersonT)
                           , _stampe :: f (TableEntity PrintT)
                           , _classi_di_materiali :: f (TableEntity MaterialsClassT)
                           } deriving (Database be, Generic)

fabLabDB :: DatabaseSettings be FabLabDB
fabLabDB = defaultDbSettings

-- people queries
-- |Function to add a person to the database using the given connection
insertPerson :: String -> String -> String -> IO ()
insertPerson cf name surname = do
                                conn <- connectPostgreSQL uri
                                runBeam putStrLn conn $ runInsert $ insert (_persone fabLabDB) $ insertValues [ Person (pack cf) (pack name) (pack surname) False False False 0.0 ]

-- materials queries
-- |Function to add a person to the database using the given connection
insertMaterialsClass :: String -> String -> IO ()
insertMaterialsClass code name = do
                                    conn <- connectPostgreSQL uri
                                    runBeam putStrLn conn $ runInsert $ insert (_classi_di_materiali fabLabDB) $ insertValues [ MaterialsClass (pack code) (pack name) ]