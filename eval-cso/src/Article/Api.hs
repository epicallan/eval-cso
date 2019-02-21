-- Servant API that's composed with the rest of the api
module Article.Api
       ( ArticleApi
       , articleServer
       ) where

import Servant
-- import Control.Monad.IO.Unlift (MonadUnliftIO)

import Article.Face (ArticleReqBody, ArticleFace(..))
import Article.Backend (mkArticleBackend)
import Comment.Face (CommentReqBody)
import Model (Comment, Article)

import Config (HasConfig(..))

type ArticleApi =
          Get '[JSON] [Article]
    :<|>  ReqBody '[JSON] ArticleReqBody :> Post '[JSON] Article
    :<|> Capture "slug" Text :> "comments"
                             :> ReqBody '[JSON] CommentReqBody
                             :> Post '[JSON] Comment

articleServer
    :: forall m env. (MonadIO m, MonadReader env m, HasConfig env)
    => ServerT ArticleApi m
articleServer =
         listArticlesHandler
    :<|> createArticleHandler
    :<|>  createArticleCommentHandler
    where
        listArticlesHandler :: m [Article]
        listArticlesHandler = error "implement me"

        createArticleHandler :: ArticleReqBody -> m Article
        createArticleHandler = afCreateArticle mkArticleBackend

        createArticleCommentHandler :: Text -> CommentReqBody -> m Comment
        createArticleCommentHandler = error "implement me"
