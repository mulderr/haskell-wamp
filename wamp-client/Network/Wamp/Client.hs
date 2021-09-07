{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module      : Network.Wamp.Client
-- Description : Connection
-- Copyright   : (c) Maciej Kazulak, 2015
-- License     : MIT
-- Maintainer  : kazulakm@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- WAMP Client.
--
module Network.Wamp.Client
  ( WampApp
  , Session (..)
  , runClientWebSocket

  , subscribe
  , unsubscribe
  , publish
  , register
  , unregister
  , call
  )
where

import           Control.Concurrent.MVar
import           Control.Exception       (throwIO)
import           Data.Aeson              hiding (Result, Options)
import qualified Data.HashMap.Strict     as HM
import qualified Network.WebSockets      as WS
import qualified System.Random           as R

import           Wuss

import           Network.Wamp.Connection hiding (Session (..))
import           Network.Wamp.Messages
import           Network.Wamp.Types

import           Network.Wamp.State


type WampApp      = Session -> IO ()


-- | Current state of a client session
data Session = Session
  { sessionId                  :: SessId
  , sessionConnection          :: Connection

  , sessionSubscriptions       :: SubscriptionStore
  , sessionRegistrations       :: RegistrationStore

  , sessionPublishRequests     :: Store PublishRequest
  , sessionSubscribeRequests   :: Store SubscribeRequest
  , sessionUnsubscribeRequests :: Store UnsubscribeRequest
  , sessionCallRequests        :: Store CallRequest
  , sessionRegisterRequests    :: Store RegisterRequest
  , sessionUnregisterRequests  :: Store UnregisterRequest

  , sessionGenId               :: IO ID
  }

-- | Connection and SessionId are required. We can provide defaults for the rest.
mkSession :: Connection -> SessId -> IO Session
mkSession sessionConnection sessionId = do
  sessionSubscriptions <- mkSubscriptionStore
  sessionRegistrations <- mkRegistrationStore

  sessionPublishRequests      <- mkStore
  sessionSubscribeRequests    <- mkStore
  sessionUnsubscribeRequests  <- mkStore
  sessionCallRequests         <- mkStore
  sessionRegisterRequests     <- mkStore
  sessionUnregisterRequests   <- mkStore

  let sessionGenId = genGlobalId

  return $ Session {..}

runClientWebSocket
  :: Bool
  -> RealmUri
  -> String
  -> Int
  -> String
  -> WampApp
  -> IO ()
runClientWebSocket secure realmUri host port path app = do
  let headers = [("Sec-WebSocket-Protocol", "wamp.2.json")]
      runWSClientWith = if secure
        then runSecureClientWith host (fromIntegral port) path WS.defaultConnectionOptions headers
        else WS.runClientWith host port path WS.defaultConnectionOptions headers
  runWSClientWith (\ws -> do
    let conn = Connection
                { connectionParse     = parseWsMessage ws
                , connectionWrite     = writeWsMessage ws
                , connectionClose     = closeWsConnection ws
                }
    connect conn realmUri >>= app
    )

connect :: Connection -> RealmUri -> IO Session
connect conn realmUri = do
  sendMessage conn $ Hello realmUri (Details $ HM.fromList
    [ "roles" .= object
      [ "callee"     .= object []
      , "caller"     .= object []
      , "publisher"  .= object []
      , "subscriber" .= object []
      ]
    ])
  msg <- receiveMessage conn

  case msg of
    Welcome sessId _ -> mkSession conn sessId
    _ -> do
      _ <- throwIO $ ProtocolException $ "Unexpected message: " ++ show msg
      error "Silence! compiler."


subscribe :: Session -> TopicUri -> Options -> Handler -> IO (Result Subscription)
subscribe session topicUri opts handler = do
  reqId <- sessionGenId session >>= return . ReqId
  m <- newEmptyMVar
  insert (sessionSubscribeRequests session) $ SubscribeRequest m reqId topicUri handler
  sendMessage (sessionConnection session) $ Subscribe reqId opts topicUri
  return m

unsubscribe :: Session -> Subscription -> IO (Result Bool)
unsubscribe session sub = do
  let subId = subscriptionId sub
  reqId <- sessionGenId session >>= return . ReqId
  m <- newEmptyMVar
  insert (sessionUnsubscribeRequests session) $ UnsubscribeRequest m reqId subId
  sendMessage (sessionConnection session) $ Unsubscribe reqId subId
  return m

publish :: Session -> TopicUri -> Arguments -> ArgumentsKw -> Options -> IO (Result ())
publish session topicUri args kwArgs opts = do
  reqId <- sessionGenId session >>= return . ReqId
  sendMessage (sessionConnection session) $ Publish reqId opts topicUri args kwArgs
  newMVar $ Right ()

register :: Session -> ProcedureUri -> Endpoint -> Options -> IO (Result Registration)
register session procedureUri endpoint opts = do
  reqId <- sessionGenId session >>= return . ReqId
  m <- newEmptyMVar
  insert (sessionRegisterRequests session) $ RegisterRequest m reqId procedureUri endpoint
  sendMessage (sessionConnection session) $ Register reqId opts procedureUri
  return m


unregister :: Session -> Registration -> IO (Result Bool)
unregister session reg = do
  let regId = registrationId reg
  reqId <- sessionGenId session >>= return . ReqId
  m <- newEmptyMVar
  insert (sessionUnregisterRequests session) $ UnregisterRequest m reqId regId
  sendMessage (sessionConnection session) $ Unregister reqId regId
  return m

call :: Session -> ProcedureUri -> Arguments -> ArgumentsKw -> Options -> IO (Result CallResult)
call session procedureUri args kwArgs opts = do
  reqId <- sessionGenId session >>= return . ReqId
  m <- newEmptyMVar
  insert (sessionCallRequests session) $ CallRequest m reqId
  sendMessage (sessionConnection session) $ Call reqId opts procedureUri args kwArgs
  return m


genGlobalId :: IO ID
genGlobalId = R.randomRIO (0, 2^(53 :: Int))
