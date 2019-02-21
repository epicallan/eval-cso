module Common.Types
       ( Id (..)
       , Name (..)
       ) where

import Data.Aeson.Options as AO (defaultOptions)
import Data.Aeson.TH (Options(..), deriveJSON)
import Database.Persist.Sql (PersistField)

newtype Name = Name {unName :: Text}
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions  { unwrapUnaryRecords = True } ''Name)

newtype Id = Id {unId :: Int64}
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions  { unwrapUnaryRecords = True }  ''Id)

