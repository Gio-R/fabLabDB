{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |Module used for defining the database schema
module Schema where

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Data.Int (Int)
import Data.Scientific
import Data.Text
import Data.Time.Calendar
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Schema.Tables

-- datatypes
-- |Data representing a person in the database
data PersonT f
  = Person
      { _personCf :: Columnar f Text,
        _personNome :: Columnar f Text,
        _personCognome :: Columnar f Text,
        _personSocio :: Columnar f Bool,
        _personOperatoreIntagliatrice :: Columnar f Bool,
        _personOperatoreStampante :: Columnar f Bool,
        _personSpesaTotale :: Columnar f Scientific
        }
  deriving (Beamable, Generic)

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
data PrintT f
  = Print
      { _printCodiceStampa :: Columnar f Int,
        _printDataRichiesta :: Columnar f Day,
        _printDataConsegna :: Columnar f (Maybe Day),
        _printTempo :: Columnar f (Maybe Double),
        _printCostoMateriali :: Columnar f (Maybe Scientific),
        _printCostoTotale :: Columnar f (Maybe Scientific),
        _printDescrizione :: Columnar f Text,
        _printCfRichiedente :: PrimaryKey PersonT f,
        _printCfIncaricato :: PrimaryKey PersonT (Nullable f),
        _printCodiceStampante :: PrimaryKey PrinterT (Nullable f)
        }
  deriving (Beamable, Generic)

instance Table PrintT where

  data PrimaryKey PrintT f = PrintId (Columnar f Int) deriving (Beamable, Generic)

  primaryKey = PrintId . _printCodiceStampa

type Print = PrintT Identity

type PrintId = PrimaryKey PrintT Identity

deriving instance Eq Print

deriving instance Show Print

deriving instance Eq PrintId

deriving instance Show PrintId

-- |Data representing a cut
data CutT f
  = Cut
      { _cutCodiceIntaglio :: Columnar f Int,
        _cutDataRichiesta :: Columnar f Day,
        _cutDataConsegna :: Columnar f (Maybe Day),
        _cutTempo :: Columnar f (Maybe Double),
        _cutCostoMateriali :: Columnar f (Maybe Scientific),
        _cutCostoTotale :: Columnar f (Maybe Scientific),
        _cutDescrizione :: Columnar f Text,
        _cutCfRichiedente :: PrimaryKey PersonT f,
        _cutCfIncaricato :: PrimaryKey PersonT (Nullable f)
        }
  deriving (Beamable, Generic)

instance Table CutT where

  data PrimaryKey CutT f = CutId (Columnar f Int) deriving (Beamable, Generic)

  primaryKey = CutId . _cutCodiceIntaglio

type Cut = CutT Identity

type CutId = PrimaryKey CutT Identity

deriving instance Eq Cut

deriving instance Show Cut

deriving instance Eq CutId

deriving instance Show CutId

-- |Data representing a printer
data PrinterT f
  = Printer
      { _printerCodiceStampante :: Columnar f Text,
        _printerMarca :: Columnar f Text,
        _printerModello :: Columnar f Text,
        _printerDescrizione :: Columnar f Text
        }
  deriving (Beamable, Generic)

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
data PlasticT f
  = Plastic
      { _plasticCodicePlastica :: Columnar f Text,
        _plasticNome :: Columnar f Text,
        _plasticDescrizione :: Columnar f Text
        }
  deriving (Beamable, Generic)

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
data FilamentT f
  = Filament
      { _filamentCodiceFilamento :: Columnar f Text,
        _filamentCodicePlastica :: PrimaryKey PlasticT f,
        _filamentMarca :: Columnar f Text,
        _filamentColore :: Columnar f Text
        }
  deriving (Beamable, Generic)

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
data ProcessingT f
  = Processing
      { _processingCodiceTipo :: PrimaryKey TypeT f,
        _processingCodiceLavorazione :: Columnar f Text,
        _processingCodiceMateriale :: PrimaryKey MaterialT f,
        _processingPotenzaMassima :: Columnar f Int,
        _processingPotenzaMinima :: Columnar f Int,
        _processingVelocita :: Columnar f Int,
        _processingDescrizione :: Columnar f Text
        }
  deriving (Beamable, Generic)

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
data TypeT f
  = Type
      { _typeCodiceTipo :: Columnar f Text,
        _typeNome :: Columnar f Text,
        _typeDescrizione :: Columnar f Text
        }
  deriving (Beamable, Generic)

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
data MaterialT f
  = Material
      { _materialCodiceClasse :: PrimaryKey MaterialsClassT f,
        _materialCodiceMateriale :: Columnar f Text,
        _materialNome :: Columnar f Text,
        _materialSpessore :: Columnar f Double,
        _materialDescrizione :: Columnar f Text
        }
  deriving (Beamable, Generic)

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
data MaterialsClassT f
  = MaterialsClass
      { _materialsclassCodiceClasse :: Columnar f Text,
        _materialsclassNome :: Columnar f Text
        }
  deriving (Beamable, Generic)

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
data CompositionT f
  = Composition
      { _compositionCodiceLavorazione :: PrimaryKey ProcessingT f,
        _compositionCodiceIntaglio :: PrimaryKey CutT f
        }
  deriving (Beamable, Generic)

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
data UseT f
  = Use
      { _useCodiceFilamento :: PrimaryKey FilamentT f,
        _useCodiceStampa :: PrimaryKey PrintT f
        }
  deriving (Beamable, Generic)

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
data FabLabDB f
  = FabLabDB
      { _persone :: f (TableEntity PersonT),
        _stampe :: f (TableEntity PrintT),
        _intagli :: f (TableEntity CutT),
        _stampanti :: f (TableEntity PrinterT),
        _plastiche :: f (TableEntity PlasticT),
        _filamenti :: f (TableEntity FilamentT),
        _lavorazioni :: f (TableEntity ProcessingT),
        _tipi :: f (TableEntity TypeT),
        _materiali :: f (TableEntity MaterialT),
        _classi_di_materiali :: f (TableEntity MaterialsClassT),
        _composizioni :: f (TableEntity CompositionT),
        _usi :: f (TableEntity UseT)
        }
  deriving (Database be, Generic)

fabLabDB :: DatabaseSettings be FabLabDB
fabLabDB =
  withDbModification defaultDbSettings
    dbModification
      { _stampe =
          modifyTableFields
            tableModification
              { _printCfRichiedente = PersonId (fieldNamed "cf_richiedente"),
                _printCfIncaricato = PersonId (fieldNamed "cf_incaricato"),
                _printCodiceStampante = PrinterId (fieldNamed "codice_stampante")
                },
        _intagli =
          modifyTableFields
            tableModification
              { _cutCfRichiedente = PersonId (fieldNamed "cf_richiedente"),
                _cutCfIncaricato = PersonId (fieldNamed "cf_incaricato")
                },
        _materiali =
          modifyTableFields
            tableModification
              { _materialCodiceClasse = MaterialsClassId (fieldNamed "codice_classe")
                },
        _filamenti =
          modifyTableFields
            tableModification
              { _filamentCodicePlastica = PlasticId (fieldNamed "codice_plastica")
                },
        _lavorazioni =
          modifyTableFields
            tableModification
              { _processingCodiceTipo = TypeId (fieldNamed "codice_tipo"),
                _processingCodiceMateriale = MaterialId (fieldNamed "codice_materiale")
                },
        _composizioni =
          modifyTableFields
            tableModification
              { _compositionCodiceIntaglio = CutId (fieldNamed "codice_intaglio"),
                _compositionCodiceLavorazione = ProcessingId (fieldNamed "codice_lavorazione")
                },
        _usi =
          modifyTableFields
            tableModification
              { _useCodiceFilamento = FilamentId (fieldNamed "codice_filamento"),
                _useCodiceStampa = PrintId (fieldNamed "codice_stampa")
                }
        }
