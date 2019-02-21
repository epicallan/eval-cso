-- Crud Operations
module Article.Storage.Operations
       ( createArticle
       ) where

import Article.Face (ArticleReqBody(..))
import Model (Article)

createArticle
    :: ArticleReqBody
    -> Article
createArticle = error "implement me"
