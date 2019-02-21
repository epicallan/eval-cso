module Article.Backend
       ( mkArticleBackend
       )where
-- import Control.Monad.IO.Unlift (MonadUnliftIO)

import Article.Face (ArticleFace(..))
import Article.Storage.Face (ArticleStorageFace(..))
import Article.Storage.Backend (mkArticleStorageFace)

import Config (HasConfig(..))

mkArticleBackend
    :: forall m r. (MonadIO m, MonadReader r m, HasConfig r)
    => ArticleFace m
mkArticleBackend = ArticleFace
    { afCreateArticle = asCreateArticle mkArticleStorageFace
    , aflistArticles = error "implement me"
    , afCreateArticleComment = error "implement me"
    }

