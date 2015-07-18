{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      : Network.Wamp.Broker
-- Description : Broker
-- Copyright   : (c) Maciej Kazulak, 2015
-- License     : MIT
-- Maintainer  : kazulakm@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- WAMP Broker.
--
module Network.Wamp.Broker
  ( Subscription (..)
  , SubscriptionStore

  , newSubscriptionStore
  , insertSubscription
  , lookupSubscription
  , lookupSubscriptionByTopicUri
  , lookupSubscriptionByTopicSubscriber
  , deleteSubscription
  , deleteSubscriptionBySessId
  , countSubscription
  )
where

import Control.Concurrent.MVar
import Data.IxSet
import Data.Typeable

import Network.Wamp.Connection
import Network.Wamp.Types


-- | Topic subscription as seen by a @Broker@
--
-- @Broker@ stores one subscription for each successful @Subscribe@ message.
data Subscription = Subscription
  { subscriptionId          :: !SubId
  , subscriptionSessionId   :: !SessId   -- ^ @Subscriber@ session id
  , subscriptionTopicUri    :: !TopicUri
  , subscriptionSession     :: !Session  -- ^ @Subscriber@ session
  }
  deriving (Eq, Typeable, Show)

instance Ord Subscription where
  compare x y = compare (subscriptionId x) (subscriptionId y)

instance Indexable Subscription where
  empty = ixSet
    [ ixFun $ \s -> [subscriptionId s]
    , ixFun $ \s -> [subscriptionSessionId s]
    , ixFun $ \s -> [subscriptionTopicUri s]
    ]

-- | Current subscriptions known to a @Broker@
newtype SubscriptionStore = SubscriptionStore (MVar (IxSet Subscription))


-- | Create a new 'SubscriptionStore'
newSubscriptionStore :: IO SubscriptionStore
newSubscriptionStore = do
  m <- newMVar empty
  return $ SubscriptionStore m

-- | Insert a 'Subscription' into a 'SubscriptionStore'
insertSubscription :: SubscriptionStore -> Subscription -> IO ()
insertSubscription (SubscriptionStore m) sub = do
  store <- takeMVar m
  putMVar m $ insert sub store

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

-- | Lookup a  'Subscription' by 'Network.Wamp.Types.TopicUri' and @Subscriber@ 'SessId'
lookupSubscriptionByTopicSubscriber :: SubscriptionStore -> TopicUri -> SessId -> IO (Maybe Subscription)
lookupSubscriptionByTopicSubscriber (SubscriptionStore m) topicUri sessId = do
  store <- readMVar m
  return $ getOne $ store @= sessId @= topicUri

-- | Delete a  'Subscription' by 'Network.Wamp.Types.SubId'
deleteSubscription :: SubscriptionStore -> SubId -> IO ()
deleteSubscription (SubscriptionStore m) subId = do
  store <- takeMVar m
  putMVar m $ deleteIx subId store

-- | Delete all 'Subscription's by @Subscriber@ session id.
--
-- Used for cleanup after @Subscriber@ session ends.
deleteSubscriptionBySessId :: SubscriptionStore -> SessId -> IO ()
deleteSubscriptionBySessId (SubscriptionStore m) sessId = do
  store <- takeMVar m
  let ds = toList $ store @= sessId
  putMVar m $ foldr delete store ds
   
-- | Return current registration count
countSubscription :: SubscriptionStore -> IO Int
countSubscription (SubscriptionStore m) = do
  store <- readMVar m
  return $ size store
