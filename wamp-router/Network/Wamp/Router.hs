{-# LANGUAGE OverloadedStrings  #-}

-- |
-- Module      : Network.Wamp.Router
-- Description : Router
-- Copyright   : (c) Maciej Kazulak, 2015
-- License     : MIT
-- Maintainer  : kazulakm@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- WAMP Router.
--
module Network.Wamp.Router
  ( Router (..)
  , RealmMap

  , defaultRouter
  , establishSession
  , cleanupSession
  , notifySubscribers

  , newSessionStore
  , insertSession
  , lookupSession
  , deleteSession
  )
where

import           Control.Concurrent.MVar
import           Data.Aeson
import qualified Data.HashMap.Strict   as HM
import qualified System.Random         as R

import Network.Wamp.Connection
import Network.Wamp.Types
import Network.Wamp.Messages
import Network.Wamp.Broker
import Network.Wamp.Dealer


-- | Maps 'Network.Wamp.Types.RealmUri' to a 'Router' for that @Realm@
type RealmMap = HM.HashMap RealmUri Router

-- | WAMP Router
data Router = Router
  { routerRealm           :: !RealmUri           -- ^ @Realm@ this router is responsible for
  , routerRoles           :: ![Role]             -- ^ Advertised roles: @Broker@, @Dealer@ or both
  , routerSessions        :: !SessionStore
  , routerSubscriptions   :: !SubscriptionStore
  , routerRegistrations   :: !RegistrationStore
  , routerInvocations     :: !InvocationStore
  , routerGenGlobalId     :: !(IO ID)            -- ^ Generator for global scope identifiers.
                                                 -- Example usage:
                                                 --
                                                 -- > sessId <- routerGenGlobalId router >>= return . SessId
                                                 --
  , routerGenRouterId     :: !(IO ID)            -- ^ Generator for router scope identifiers
  }

-- | Maps 'Network.Wamp.Types.SessId' to a client 'Network.Wamp.Connection.Session'
type SessionMap       = HM.HashMap SessId Session

-- | Client sessions
newtype SessionStore  = SessionStore (MVar SessionMap)


-- | Create a new 'SessionStore'
newSessionStore :: IO SessionStore
newSessionStore = do
  m <- newMVar HM.empty
  return (SessionStore m)

-- | Insert a 'Session' into a 'SessionStore'
insertSession :: SessionStore -> Session -> IO ()
insertSession (SessionStore m) session = do
  store <- takeMVar m
  putMVar m (HM.insert (sessionId session) session store)

-- | Lookup a 'Session' by 'SessId'
lookupSession :: SessionStore -> SessId -> IO (Maybe Session)
lookupSession (SessionStore m) sessId = do
  store <- readMVar m
  return $ HM.lookup sessId store

-- | Delete a 'Session' by 'SessId'
deleteSession :: SessionStore -> SessId -> IO ()
deleteSession (SessionStore m) sessId = do
  putStrLn $ "Delete: " ++ show sessId
  store <- takeMVar m
  putMVar m (HM.delete sessId store)


-- | Create a 'Router' with default configuration
defaultRouter :: RealmUri -> IO Router
defaultRouter realmUri = do
  sessions <- newSessionStore
  registrations <- newRegistrationStore
  invocations <- newInvocationStore
  subscriptions <- newSubscriptionStore
  return $ Router 
    { routerRealm         = realmUri
    , routerRoles         = [RoleBroker, RoleDealer]
    , routerSessions      = sessions
    , routerSubscriptions = subscriptions
    , routerRegistrations = registrations
    , routerInvocations   = invocations
    , routerGenGlobalId   = genGlobalId
    , routerGenRouterId   = genGlobalId
    }


-- | Establish WAMP session
--
-- Waits for a @Hello@ message. If the RealmUri is found in RealmMap returns
-- a new client Session and the 'Router' responsible for handling the requested
-- @Realm@.
establishSession :: RealmMap -> Connection -> IO (Session, Router)
establishSession realmMap conn = do
  m <- receiveMessage conn

  case m of
    Hello realm _ -> do
      case HM.lookup realm realmMap of
        Nothing -> do
          sendMessage conn $ Abort (Details dict) "wamp.error.no_such_realm"
          establishSession realmMap conn
        Just router -> do
          sessId <- routerGenGlobalId router >>= return . SessId
          let session = Session
                { sessionId         = sessId
                , sessionConnection = conn
                }

          -- TODO: send actual roles instead of fixed dict
          sendMessage conn $ Welcome sessId $ Details $ HM.fromList
            [ "roles" .= object 
              [ "broker" .= object []
              , "dealer" .= object []
              ]
            ]

          return (session, router)
    _ -> do
      -- sending anything but Hello in the first message is a protocol error
      -- we send an ABORT but dont close the underlying connection, its valid
      -- to have many WAMP sessions during one transport session so the client
      -- may try again later
      sendMessage conn $ Abort (Details dict) "wamp.error.expected_hello"
      establishSession realmMap conn


-- | Cleanup session
cleanupSession :: Router -> Session -> IO ()
cleanupSession router session = do
  let sessId = sessionId session

  -- TODO: https://github.com/tavendo/WAMP/issues/106
  -- if the client was processing an Invocation but did not Yield yet
  -- we have to send an Error to the Caller

  mapM_ ($ sessId) 
    [ deleteRegistrationBySessId (routerRegistrations router)
    , deleteInvocationByCallerSessId (routerInvocations router)
    , deleteInvocationByCalleeSessId (routerInvocations router)
    , deleteSubscriptionBySessId (routerSubscriptions router)
    , deleteSession (routerSessions router)
    ]
  putStrLn $ "Done cleaning after: " ++ show sessId

  subCount <- countSubscription (routerSubscriptions router)
  invCount <- countInvocation (routerInvocations router)
  regCount <- countRegistration (routerRegistrations router)

  putStrLn $ "Statistics:"
    ++ " subs:" ++ show subCount
    ++ " invs:" ++ show invCount
    ++ " regs:" ++ show regCount


-- | Send Event messages to all subscribers in response to a Publish
notifySubscribers 
  :: Router   -- ^ 'Router' for the @Realm@ in question
  -> PubId    -- ^ Publication id (the one from Published)
  -> Message  -- ^ Publish message
  -> IO ()
notifySubscribers router pubId pubMsg =
  case pubMsg of
    Publish _ _ topicUri args kwArgs -> do
      subs <- lookupSubscriptionByTopicUri (routerSubscriptions router) topicUri
      flip mapM_ subs (\(Subscription subId _ _ subscriberSession) -> 
        sendMessage (sessionConnection subscriberSession) $ Event subId pubId (Details dict) args kwArgs)
    _ -> error "expected a Publish message"


genGlobalId :: IO ID
genGlobalId = R.randomRIO (0, 2^(53 :: Int))
