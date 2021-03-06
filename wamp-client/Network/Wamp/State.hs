{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      : Network.Wamp.State
-- Description : Client state
-- Copyright   : (c) Maciej Kazulak, 2015
-- License     : MIT
-- Maintainer  : kazulakm@gmail.com
-- Stability   : experimental
-- Portability : portable
--
module Network.Wamp.State
  ( Subscription (..)
  , SubscriptionStore
  , Handler
  , Result

  , Store (..)
  , Storeable (..)
  , PublishRequest (..)
  , SubscribeRequest (..)
  , UnsubscribeRequest (..)
  , CallRequest (..)
  , RegisterRequest (..)
  , UnregisterRequest (..)

  , mkSubscriptionStore
  , insertSubscription
  , lookupSubscription
  , lookupSubscriptionByTopicUri
  , deleteSubscription
  , countSubscription
  )
where

import           Control.Concurrent.MVar
import           Control.Exception        (SomeException (..))
import           Data.IxSet               hiding (insert)
import qualified Data.IxSet               as Ix (insert)
import           Data.Typeable

import Network.Wamp.Types

-- | Promise for a result
type Result a = MVar (Either SomeException a)

-- | Event handler
type Handler = Arguments -> ArgumentsKw -> Details -> IO ()


-- | Topic subscription as seen by a @Subscriber@
--
-- @Subscriber@ stores one subscription for each successful @Subscribed@ message.
data Subscription = Subscription 
  { subscriptionId       :: SubId
  , subscriptionTopicUri :: TopicUri
  , subscriptionHandler  :: Handler
  , subscriptionOptions  :: Options
  }
  deriving (Typeable)

instance Eq Subscription where
  x == y = (subscriptionId x) == (subscriptionId y)

instance Ord Subscription where
  compare x y = compare (subscriptionId x) (subscriptionId y)

instance Show Subscription where
  show (Subscription subId topicUri _ _) = "Subscription " ++ show subId ++ " " ++ show topicUri

instance Indexable Subscription where
  empty = ixSet
    [ ixFun $ \s -> [subscriptionId s]
    , ixFun $ \s -> [subscriptionTopicUri s]
    ]


-- | Current subscriptions known to a @Subscriber@
newtype SubscriptionStore = SubscriptionStore (MVar (IxSet Subscription))


-- | Create a new 'SubscriptionStore'
mkSubscriptionStore :: IO SubscriptionStore
mkSubscriptionStore = do
  m <- newMVar empty
  return $ SubscriptionStore m

-- | Insert a 'Subscription' into a 'SubscriptionStore'
insertSubscription :: SubscriptionStore -> Subscription -> IO ()
insertSubscription (SubscriptionStore m) sub = do
  store <- takeMVar m
  putMVar m $ Ix.insert sub store

-- | Lookup a  'Subscription' by 'Network.Wamp.Types.SubId'
lookupSubscription :: SubscriptionStore -> SubId -> IO (Maybe Subscription)
lookupSubscription (SubscriptionStore m) subId = do
  store <- readMVar m
  return $ getOne $ store @= subId

-- | Lookup a  'Subscription' by 'Network.Wamp.Types.TopicUri'
lookupSubscriptionByTopicUri :: SubscriptionStore -> TopicUri -> IO [Subscription]
lookupSubscriptionByTopicUri (SubscriptionStore m) topicUri = do
  store <- readMVar m
  return $ toList $ store @= topicUri

-- | Delete a  'Subscription' by 'Network.Wamp.Types.SubId'
deleteSubscription :: SubscriptionStore -> SubId -> IO ()
deleteSubscription (SubscriptionStore m) subId = do
  store <- takeMVar m
  putMVar m $ deleteIx subId store
   
-- | Return current registration count
countSubscription :: SubscriptionStore -> IO Int
countSubscription (SubscriptionStore m) = do
  store <- readMVar m
  return $ size store


data Store a = Store (MVar (IxSet a))

class (Eq s, Ord s, Indexable s, Typeable s) => Storeable s where
  mkStore :: IO (Store s)
  mkStore = do
    m <- newMVar empty
    return $ Store m

  insert :: Store s -> s -> IO ()
  insert (Store m) s = do
    store <- takeMVar m
    putMVar m $ Ix.insert s store

  lookup :: Store s -> ReqId -> IO (Maybe s)
  lookup (Store m) reqId = do
    store <- readMVar m
    return $ getOne $ store @= reqId

  delete :: Store s -> ReqId -> IO ()
  delete (Store m) reqId = do
    store <- takeMVar m
    putMVar m $ deleteIx reqId store

  count :: Store s -> IO Int
  count (Store m) = do
    store <- readMVar m
    return $ size store


instance Storeable PublishRequest
instance Storeable SubscribeRequest
instance Storeable UnsubscribeRequest
instance Storeable CallRequest
instance Storeable RegisterRequest
instance Storeable UnregisterRequest


-- | Publish request
data PublishRequest = PublishRequest
  { publishPromise         :: Result ()
  , publishRequestId       :: ReqId
  }
  deriving (Typeable)

instance Eq PublishRequest where
  x == y = (publishRequestId x) == (publishRequestId y)

instance Ord PublishRequest where
  compare x y = compare (publishRequestId x) (publishRequestId y)

instance Show PublishRequest where
  show p = "PublishRequest " ++ show (publishRequestId p)

instance Indexable PublishRequest where
  empty = ixSet
    [ ixFun $ \s -> [publishRequestId s]
    ]


-- | Subscribe request
data SubscribeRequest = SubscribeRequest
  { subscribePromise         :: Result Subscription
  , subscribeRequestId       :: ReqId
  , subscribeRequestTopicUri :: TopicUri
  , subscribeRequestHandler  :: Handler
  }
  deriving (Typeable)

instance Eq SubscribeRequest where
  x == y = (subscribeRequestId x) == (subscribeRequestId y)

instance Ord SubscribeRequest where
  compare x y = compare (subscribeRequestId x) (subscribeRequestId y)

instance Show SubscribeRequest where
  show s = "SubscribeRequest " ++ show (subscribeRequestId s)

instance Indexable SubscribeRequest where
  empty = ixSet
    [ ixFun $ \s -> [subscribeRequestId s]
    ]


-- | Unsubscribe request
data UnsubscribeRequest = UnsubscribeRequest
  { unsubscribePromise         :: Result Bool
  , unsubscribeRequestId       :: ReqId
  , unsubscribeRequestSubId    :: SubId
  }
  deriving (Typeable)

instance Eq UnsubscribeRequest where
  x == y = (unsubscribeRequestId x) == (unsubscribeRequestId y)

instance Ord UnsubscribeRequest where
  compare x y = compare (unsubscribeRequestId x) (unsubscribeRequestId y)

instance Show UnsubscribeRequest where
  show usub = "UnsubscribeRequest " ++ show (unsubscribeRequestId usub)

instance Indexable UnsubscribeRequest where
  empty = ixSet
    [ ixFun $ \s -> [unsubscribeRequestId s]
    ]


-- | Call request
data CallRequest = CallRequest
  { callRequestId       :: ReqId
  , callRequestSubId    :: SubId
  }
  deriving (Typeable)

instance Eq CallRequest where
  x == y = (callRequestId x) == (callRequestId y)

instance Ord CallRequest where
  compare x y = compare (callRequestId x) (callRequestId y)

instance Show CallRequest where
  show (CallRequest reqId _) = "CallRequest " ++ show reqId

instance Indexable CallRequest where
  empty = ixSet
    [ ixFun $ \s -> [callRequestId s]
    ]

-- | Register request
data RegisterRequest = RegisterRequest
  { registerRequestId       :: ReqId
  , registerRequestProcUri  :: ProcedureUri 
  --, registerRequestEndpoint :: Endpoint 
  , registerRequestOptions  :: Options
  }
  deriving (Typeable)

instance Eq RegisterRequest where
  x == y = (registerRequestId x) == (registerRequestId y)

instance Ord RegisterRequest where
  compare x y = compare (registerRequestId x) (registerRequestId y)

instance Show RegisterRequest where
  show (RegisterRequest reqId _ _) = "RegisterRequest " ++ show reqId

instance Indexable RegisterRequest where
  empty = ixSet
    [ ixFun $ \s -> [registerRequestId s]
    ]

-- | Unregister request
data UnregisterRequest = UnregisterRequest
  { unregisterRequestId       :: ReqId
  , unregisterRequestSubId    :: SubId
  }
  deriving (Typeable)

instance Eq UnregisterRequest where
  x == y = (unregisterRequestId x) == (unregisterRequestId y)

instance Ord UnregisterRequest where
  compare x y = compare (unregisterRequestId x) (unregisterRequestId y)

instance Show UnregisterRequest where
  show (UnregisterRequest reqId _) = "UnregisterRequest " ++ show reqId

instance Indexable UnregisterRequest where
  empty = ixSet
    [ ixFun $ \s -> [unregisterRequestId s]
    ]
