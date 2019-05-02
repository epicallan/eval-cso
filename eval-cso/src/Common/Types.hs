module Common.Types
       ( Id (..)
       ) where

import Data.Aeson.Options as AO (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Database.Persist.Sql (PersistField)

newtype Id = Id {unId :: Int64}
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions ''Id)
