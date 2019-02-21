module Tag.Face
        ( Tag (..)
        , TagFace (..)
        ) where

import Data.Aeson.TH (deriveJSON, Options (..))
import qualified Data.Aeson.Options as AO (defaultOptions)
import Database.Persist.Sql (PersistField, PersistFieldSql)

newtype Tag = Tag {unTag :: Text } deriving (Eq, Show, PersistField, PersistFieldSql)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''Tag)

newtype TagFace m = TagFace { tfListTags :: m [Tag] }
