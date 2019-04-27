module Evaluation.Types
        ( Category (..)
        , CustomerNumber (..)
        , Comment (..)
        , CreateEvaluation (..)
        , Duration (..)
        , Description (..)
        , EvalRecord (..)
        , EvalAttrs (..)
        , Group (..)
        , HasParameterAttrs (..)
        , HasCreateEvaluation (..)
        , HasEvalRecord (..)
        , HasServiceParameters (..)
        , ParameterAttrs (..)
        , ParaName (..)
        , Paravalue (..)
        , Reason (..)
        , ServiceAttrs (..)
        , ServiceType (..)
        , ServiceTypeValue (..)
        , ServiceParameters (..)
        , EvalErrors (..)
        , Score (..)
        , Weight (..)
        ) where

import Data.Aeson.Options as AO (defaultOptions)
import Data.Aeson.TH (Options(..), deriveJSON)
import Data.Time (UTCTime)
import Database.Persist.Sql (PersistField)
import Database.Persist.TH (derivePersistField)
import qualified GHC.Show (Show(show))
import Lens.Micro.Platform (makeClassy)

import User.Types (UserName)

data Category = ZeroRated | NonZeroRated
  deriving (Eq, Read)

instance Show Category where
  show ZeroRated = "Reasons for zero rating"
  show NonZeroRated = "Reasons for deviation"

$(deriveJSON AO.defaultOptions ''Category)

derivePersistField "Category"

newtype ParaName = ParaName { unParaName :: Text }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''ParaName)

newtype Group = Group { unGroup :: Text }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''Group)

newtype Comment = Comment { unComment :: Text }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''Comment)

newtype Description = Description { unDescription :: Text }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''Description)

newtype Reason = Reason { unReason :: Text }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''Reason)

newtype CustomerNumber = CustomerNumber {unCustomerNumber :: Int }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''CustomerNumber)

newtype Weight = Weight {unWeight :: Int }
  deriving (Eq, Show, PersistField, Num)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''Weight)

newtype Duration = Duration {unDuration :: Int }
  deriving (Eq, Show, PersistField, Num)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''Duration)

newtype Score = Score {unScore :: Int }
  deriving (Eq, Show, Num)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''Score)

newtype ServiceType = ServiceType { unServiceType :: Text }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''ServiceType)

newtype ServiceTypeValue = ServiceTypeValue { unServiceTypeValue :: Text }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''ServiceTypeValue)

data ServiceAttrs = ServiceAttrs
  { saName :: ServiceType
  , saValue :: ServiceTypeValue
  } deriving (Show)
$(deriveJSON AO.defaultOptions ''ServiceAttrs)

newtype Paravalue = Paravalue { unPvalue :: Text }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''Paravalue)

data EvalErrors =
    EServiceNotFound ServiceTypeValue
  | EUserNameNotFound UserName
  | EParameterNotFound Paravalue
  | ActionIsForEvaluatorsOnly UserName
  deriving Show

instance Exception EvalErrors

data ParameterAttrs = ParameterAttrs
  { _paName :: ParaName
  , _paValue :: Paravalue
  , _paDescription :: Maybe Description
  , _paWeight :: Weight
  , _paGroup :: Maybe Group
  , _paCategory :: Category
  } deriving (Show)

$(deriveJSON AO.defaultOptions ''ParameterAttrs)
makeClassy ''ParameterAttrs

data EvalAttrs = EvalAttrs
  { _eaReason :: Reason
  , _eaEvaluator :: UserName
  , _eaAgent :: UserName
  , _eaService :: ServiceTypeValue
  , _eaCustomer :: CustomerNumber
  , _eaComment :: Maybe Comment
  , _eaDuration :: Maybe Duration
  , _eaDate :: UTCTime
  } deriving (Show)

$(deriveJSON AO.defaultOptions ''EvalAttrs)
makeClassy ''EvalAttrs

data EvalRecord = EvalRecord
  { _erParameters :: [ParameterAttrs]
  , _erEvalAttrs :: EvalAttrs
  , _erScore :: Weight
  } deriving (Show)

$(deriveJSON AO.defaultOptions ''EvalRecord)
makeClassy ''EvalRecord

data CreateEvaluation = CreateEvaluation
  { _ceEvalAttrs :: EvalAttrs
  , _ceParameters :: [Paravalue]
  } deriving (Show)

$(deriveJSON AO.defaultOptions ''CreateEvaluation)
makeClassy ''CreateEvaluation

data ServiceParameters = ServiceParameters
  { _spService :: ServiceTypeValue
  , _spParameters :: [ParameterAttrs]
  } deriving (Show)

$(deriveJSON AO.defaultOptions ''ServiceParameters)
makeClassy ''ServiceParameters
