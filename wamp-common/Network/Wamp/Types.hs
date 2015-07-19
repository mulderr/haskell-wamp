{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
--

-- | Session ID.
newtype SessId = SessId ID
  deriving (Eq, Ord, Show, Typeable, FromJSON, ToJSON, Hashable)

-- | Request ID.
newtype ReqId = ReqId ID
  deriving (Eq, Ord, Show, Typeable, FromJSON, ToJSON, Hashable)

-- | Publication ID.
newtype PubId = PubId ID
  deriving (Eq, Ord, Show, Typeable, FromJSON, ToJSON, Hashable)

-- | Subscription ID.
newtype SubId = SubId ID
  deriving (Eq, Ord, Show, Typeable, FromJSON, ToJSON, Hashable)

-- | Registration ID.
newtype RegId = RegId ID
  deriving (Eq, Ord, Show, Typeable, FromJSON, ToJSON, Hashable)


-- | Realm URI.
newtype RealmUri = RealmUri URI
  deriving (Eq, Ord, Show, Typeable, FromJSON, ToJSON, Hashable, IsString)

-- | Reason URI.
newtype ReasonUri = ReasonUri URI
  deriving (Eq, Ord, Show, Typeable, FromJSON, ToJSON, Hashable, IsString)

-- | Error URI.
newtype ErrorUri = ErrorUri URI
  deriving (Eq, Ord, Show, Typeable, FromJSON, ToJSON, Hashable, IsString)

-- | Topic URI.
newtype TopicUri = TopicUri URI
  deriving (Eq, Ord, Show, Typeable, FromJSON, ToJSON, Hashable, IsString)

-- | Procedure URI.
newtype ProcedureUri = ProcedureUri URI
  deriving (Eq, Ord, Show, Typeable, FromJSON, ToJSON, Hashable, IsString)


-- | Arguments list.
newtype Arguments = Arguments Array
  deriving (Eq, Show, Typeable, FromJSON, ToJSON)

-- | Arguments dictionary.
newtype ArgumentsKw = ArgumentsKw Dict
  deriving (Eq, Show, Typeable, FromJSON, ToJSON)

-- | Details dictionary.
newtype Details = Details Dict
  deriving (Eq, Show, Typeable, FromJSON, ToJSON)

-- | Options dictionary.
newtype Options = Options Dict
  deriving (Eq, Show, Typeable, FromJSON, ToJSON)


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
