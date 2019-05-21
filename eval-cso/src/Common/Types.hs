module Common.Types
       ( Id (..)
       , RecordId (..)
       ) where
import Data.Aeson (Options(..))
import Data.Aeson.Options as AO (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Database.Persist.Sql (PersistField)
import Web.HttpApiData (FromHttpApiData)

newtype Id = Id {unId :: Int64}
  deriving (Eq, Show, PersistField, FromHttpApiData)

$(deriveJSON AO.defaultOptions ''Id)

newtype RecordId = RecordId {unRecordId :: Int64}
  deriving (Eq, Show)

$(deriveJSON AO.defaultOptions  { unwrapUnaryRecords = True } ''RecordId)
