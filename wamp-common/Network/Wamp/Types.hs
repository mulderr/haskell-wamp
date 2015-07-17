{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

-- |
-- Module      : Network.Wamp.Types
-- Copyright   : (c) Maciej Kazulak, 2015
-- License     : MIT
-- Maintainer  : kazulakm@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- Entities defined in the protocol specification. Mainly 'Network.Wamp.Messages.Message' building blocks.
--
module Network.Wamp.Types
  ( URI
  , ID
  , Dict

  , SessId (..)
  , ReqId (..)
  , PubId (..)
  , SubId (..)
  , RegId (..)
  , RealmUri (..)
  , ReasonUri (..)
  , ErrorUri (..)
  , TopicUri (..)
  , ProcedureUri (..)
  , Arguments (..)
  , Details (..)
  , ArgumentsKw (..)
  , Options (..)

  , Role (..)
  , MessageType (..)
  
  , dict
  , array
  )
where

import           Data.Int              (Int64)
import           Data.Aeson
import           Data.Hashable         (Hashable)
import qualified Data.HashMap.Strict   as HM
import qualified Data.Vector           as V
import           Data.Maybe            (fromJust)
import           Data.String           (IsString (..))
import           Data.Text             (Text)
import           Data.Typeable
import           GHC.Generics


-- Protocol specification:
-- https://github.com/tavendo/WAMP/blob/master/spec/basic.md

-- | Internal representation for ID types.
--
-- WAMP IDs are integers between (inclusive) 0 and 2^53.
type ID = Int64  


-- | Internal representation for URI types.
--
-- Must be able to hold UTF-8 data.
type URI = Text               
     

-- | Internal representation for dict types.
type Dict = HM.HashMap Text Value


--
-- Types used in messages.
--
-- Ord and Typeable are required to create an index using ixset's ixFun.
-- Generic is required to automatically derive FromJSON and ToJSON.
--

-- | Session ID.
newtype SessId = SessId ID
  deriving (Eq, Generic, Ord, Show, Typeable)

instance FromJSON SessId
instance ToJSON SessId
instance Hashable SessId

-- | Request ID.
newtype ReqId = ReqId ID
  deriving (Eq, Generic, Ord, Show, Typeable)

instance FromJSON ReqId
instance ToJSON ReqId
instance Hashable ReqId

-- | Publication ID.
newtype PubId = PubId ID
  deriving (Eq, Generic, Ord, Show, Typeable)

instance FromJSON PubId
instance ToJSON PubId

-- | Subscription ID.
newtype SubId = SubId ID
  deriving (Eq, Generic, Ord, Show, Typeable)

instance FromJSON SubId
instance ToJSON SubId

-- | Registration ID.
newtype RegId = RegId ID
  deriving (Eq, Generic, Ord, Show, Typeable)

instance FromJSON RegId
instance ToJSON RegId


-- | Realm URI.
newtype RealmUri = RealmUri URI
  deriving (Eq, Generic, Ord, Show, Typeable)

instance FromJSON RealmUri
instance ToJSON RealmUri
instance Hashable RealmUri
instance IsString RealmUri where
  fromString = RealmUri . fromString

-- | Reason URI.
newtype ReasonUri = ReasonUri URI
  deriving (Eq, Generic, Ord, Show, Typeable)

instance FromJSON ReasonUri
instance ToJSON ReasonUri
instance IsString ReasonUri where
  fromString = ReasonUri . fromString

-- | Error URI.
newtype ErrorUri = ErrorUri URI
  deriving (Eq, Generic, Ord, Show, Typeable)

instance FromJSON ErrorUri
instance ToJSON ErrorUri
instance IsString ErrorUri where
  fromString = ErrorUri . fromString

-- | Topic URI.
newtype TopicUri = TopicUri URI
  deriving (Eq, Generic, Ord, Show, Typeable)

instance FromJSON TopicUri
instance ToJSON TopicUri
instance IsString TopicUri where
  fromString = TopicUri . fromString

-- | Procedure URI.
newtype ProcedureUri = ProcedureUri URI
  deriving (Eq, Generic, Ord, Show, Typeable)

instance FromJSON ProcedureUri
instance ToJSON ProcedureUri
instance IsString ProcedureUri where
  fromString = ProcedureUri . fromString


-- | Arguments list.
newtype Arguments = Arguments Array
  deriving (Eq, Generic, Show, Typeable)

instance FromJSON Arguments
instance ToJSON Arguments

-- | Arguments dictionary.
newtype ArgumentsKw = ArgumentsKw Dict
  deriving (Eq, Generic, Show, Typeable)

instance FromJSON ArgumentsKw
instance ToJSON ArgumentsKw

-- | Details dictionary.
newtype Details = Details Dict
  deriving (Eq, Generic, Show, Typeable)

instance FromJSON Details
instance ToJSON Details

-- | Options dictionary.
newtype Options = Options Dict
  deriving (Eq, Generic, Show, Typeable)

instance FromJSON Options
instance ToJSON Options


data Role
  = RoleCallee
  | RoleCaller
  | RolePublisher
  | RoleSubscriber
  | RoleDealer
  | RoleBroker
  deriving (Eq, Show)


-- | Message types.
data MessageType
  = MsgTypeHello
  | MsgTypeWelcome 
  | MsgTypeAbort
  | MsgTypeChallenge
  | MsgTypeAuthenticate
  | MsgTypeGoodbye
  | MsgTypeError
  | MsgTypePublish
  | MsgTypePublished
  | MsgTypeSubscribe
  | MsgTypeSubscribed
  | MsgTypeUnsubscribe
  | MsgTypeUnsubscribed
  | MsgTypeEvent
  | MsgTypeCall
  | MsgTypeCancel
  | MsgTypeResult
  | MsgTypeRegister
  | MsgTypeRegistered
  | MsgTypeUnregister
  | MsgTypeUnregistered
  | MsgTypeInvocation
  | MsgTypeInterrupt
  | MsgTypeYield
  deriving (Eq, Show)


messageCodes :: [(MessageType, Int)]
messageCodes =
  [ (MsgTypeHello,           1)
  , (MsgTypeWelcome,         2)
  , (MsgTypeAbort,           3)
  , (MsgTypeChallenge,       4)
  , (MsgTypeAuthenticate,    5)
  , (MsgTypeGoodbye,         6)
  , (MsgTypeError,           8)
  , (MsgTypePublish,        16)
  , (MsgTypePublished,      17)
  , (MsgTypeSubscribe,      32)
  , (MsgTypeSubscribed,     33)
  , (MsgTypeUnsubscribe,    34)
  , (MsgTypeUnsubscribed,   35)
  , (MsgTypeEvent,          36)
  , (MsgTypeCall,           48)
  , (MsgTypeCancel,         49)
  , (MsgTypeResult,         50)
  , (MsgTypeRegister,       64)
  , (MsgTypeRegistered,     65)
  , (MsgTypeUnregister,     66)
  , (MsgTypeUnregistered,   67)
  , (MsgTypeInvocation,     68)
  , (MsgTypeInterrupt,      69)
  , (MsgTypeYield,          70)
  ]


instance Enum MessageType where
  fromEnum = fromJust . flip lookup messageCodes
  toEnum = fromJust . flip lookup (map (\(a, b) -> (b, a)) messageCodes)

instance ToJSON MessageType where
  toJSON = toJSON . fromEnum

instance FromJSON MessageType where
  parseJSON x = parseJSON x >>= return . toEnum


-- | Empty list.
array :: Array
array = V.empty

-- | Empty dict.
dict :: Dict
dict = HM.empty
