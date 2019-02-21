module Article.Face
        ( ArticleFace (..)
        , ArticleReqBody (..)
        ) where
import Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Options as AO (defaultOptions)

import Article.Core (Body, Description, Title)
import Model (Article)
import Tag.Face (Tag)

data ArticleReqBody = ArticleReqBody
    { arTitle :: Title
    , arDescription :: Description
    , arBody :: Body
    , arTagList :: [Tag]
    } deriving (Eq, Show)

$(deriveJSON AO.defaultOptions ''ArticleReqBody)

data ArticleFace m = ArticleFace
    { afCreateArticle :: ArticleReqBody -> m Article-- TODO: switch from Maybe
    , aflistArticles :: m [Article]
    , afCreateArticleComment :: m ()
    }
