module Comment.Face (CommentReqBody) where

import Data.Aeson.TH (deriveJSON, Options (..))
import qualified Data.Aeson.Options as AO (defaultOptions)

import Article.Core (Body) -- TODO: Should be in Common.Core

newtype CommentReqBody = CommentReqBody
    { crBody :: Body
    } deriving (Eq, Show)

$(deriveJSON AO.defaultOptions  { unwrapUnaryRecords = True } ''CommentReqBody)
