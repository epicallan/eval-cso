module Article.Core
        ( Description (..)
        , Slug (..)
        , Body (..)
        , Title (..)
        ) where

import Data.Aeson.TH (deriveJSON, Options (..))
import qualified Data.Aeson.Options as AO (defaultOptions)
import Database.Persist.Sql (PersistField, PersistFieldSql)

newtype Title = Title {unTitle :: Text } deriving (Show, Eq, Ord, PersistField, PersistFieldSql)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''Title)

newtype Description = Description {unDescription :: Text }
    deriving (Show, Eq, Ord, PersistField, PersistFieldSql)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''Description)

newtype Slug = Slug {unSlug :: Text } deriving (Show, Eq, Ord, PersistField, PersistFieldSql)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''Slug)

newtype Body = Body {unBody :: Text }
    deriving (Show, Eq, Ord, PersistField, PersistFieldSql)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''Body)

