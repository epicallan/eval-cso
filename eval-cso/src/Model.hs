{-# LANGUAGE EmptyDataDecls, QuasiQuotes #-}

module Model
  ( -- auto generated persistent Field types e.g UserName
    EntityField (..)

  , Article
  , ArticleId
  , Comment
  , CommentId
  , User (..)
  , UserId

  , doMigrations
  , runInDb
  ) where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Time (UTCTime)
import Database.Persist.Sql (EntityField, SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH
  (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

-- import Lens.Micro.Platform (camelCaseFields, makeLensesWith)

-- import Article.Core (Body, Description, Slug, Title)
import Common.Types (Name)
import Config (HasPool(..))
import User.Types as U (Email, Role)


share
  [mkPersist sqlSettings
  , mkMigrate "migrateAll"
  ] [persistLowerCase|
  User sql=users
    name           Name     sqltype=text
    email          U.Email  sqltype=text
    role           U.Role   sqltype=text
    createdAt      UTCTime  sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    updatedAt      UTCTime  sqltype=timestamptz sql=updated_at default=CURRENT_TIMESTAMP
    UniqueEmail    email
    deriving Show

  Article sql=articles
    createdAt      UTCTime  sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    updatedAt      UTCTime  sqltype=timestamptz sql=updated_at default=CURRENT_TIMESTAMP
   deriving Show

  Comment sql=comments
    createdAt      UTCTime  sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    updatedAt      UTCTime  sqltype=timestamptz sql=updated_at default=CURRENT_TIMESTAMP
   deriving Show

  |]

$(deriveJSON defaultOptions ''Article)
$(deriveJSON defaultOptions ''Comment)
$(deriveJSON defaultOptions ''User)

-- makeLensesWith camelCaseFields ''User

-- we should be able to run multiple migrations
doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runInDb
  :: (MonadIO m, MonadReader r m, HasPool r)
  => SqlPersistT IO a -> m a
runInDb query = do
    cPool <- view pool
    liftIO $ runSqlPool query cPool
