-- Storage CRUD operations
module Article.Storage.Backend
       ( mkArticleStorageFace  
       ) where

-- import Prelude hiding (get)

-- import Database.Persist.Postgresql (insert, get)
-- import Control.Monad.IO.Unlift (MonadUnliftIO)

import Article.Storage.Face (ArticleStorageFace(..))
import Article.Storage.Operations (createArticle)

import Config (HasConfig(..))
-- import Model (runInDb)

mkArticleStorageFace
    :: forall m r. (MonadIO m, MonadReader r m, HasConfig r)
    => ArticleStorageFace m
mkArticleStorageFace =
    ArticleStorageFace
        {
        asCreateArticle   =  \reqBody -> do
            let article = createArticle reqBody
            putTextLn . show $ article
            appName <- view (config . cAppName)
            putTextLn . show $ appName
            error "TODO:"
        , asUpdateArticle   = error "Do me"
        , asFindArticleById = error "Do me"
        , asListArticles    = error "Do me"
        , asDeleteArticle   = error "Do me"
        }

-- TODO: make test Face
