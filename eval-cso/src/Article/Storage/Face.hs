module Article.Storage.Face
       ( ArticleStorageFace (..)
       , UnknownArticleId
       , ArticleId (..) -- TODO: could come from model
       ) where

import Model (Article)
import Article.Face (ArticleReqBody)
-- import Model (User)

data UnknownArticleId = UnknownArticleId deriving (Show)

instance Exception UnknownArticleId

newtype ArticleId = ArticleId {unArticleId :: Int } deriving (Show) -- TODO: could be got from Model

data ArticleStorageFace m = ArticleStorageFace
    { asCreateArticle   :: ArticleReqBody -> m Article
    , asUpdateArticle   :: ArticleId -> m (Either UnknownArticleId Article)
    , asFindArticleById :: ArticleId -> m (Either UnknownArticleId Article)
    , asListArticles    :: m [Article]
    , asDeleteArticle   :: ArticleId -> m (Either UnknownArticleId Article)
    }
