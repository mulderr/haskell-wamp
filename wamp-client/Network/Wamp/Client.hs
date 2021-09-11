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
  , Session
  , runClientWebSocket

  , subscribe
  , unsubscribe
  , publish
  , publishAck
  , register
  , unregister
  , call
  , trySubscribe
  , tryUnsubscribe
  , tryPublishAck
  , tryRegister
  , tryUnregister
  , tryCall
  )
where

import           Control.Concurrent.MVar
import           Control.Exception       (throwIO, try, SomeException, Exception, toException)
import           Data.Aeson              hiding (Result, Options, Error)
import qualified Data.HashMap.Strict     as HM
import qualified Network.WebSockets      as WS
import qualified System.Random           as R
import qualified Data.IxSet              as Ix
import qualified Data.Vector             as V
import qualified Data.Text               as T

import Control.Concurrent.Async(race, forConcurrently_)
import Control.Concurrent(forkFinally)
import Control.Monad

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
    session <- connect conn realmUri
    app session `race` readerLoop session
    return ()
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
    _ -> throwIO $ ProtocolException $ "Unexpected message: " ++ show msg

readerLoop :: Session -> IO ()
readerLoop session = go
  where conn = sessionConnection session
        go = do
          msg <- receiveMessage conn
          case msg of
            Abort {} -> finalizeSession session

            Goodbye {} -> do
              finalizeSession session
              sendMessage conn $ Goodbye (Details HM.empty) "wamp.close.goodbye_and_out"

            Error msgType reqId details errorUri args argskw -> do
              let fail' :: (Storeable a, HasPromise a b, Exception e) =>
                           (Session -> Store a) -> e -> IO ()
                  fail' reqStore x = do
                    mr <- extract (reqStore session) reqId
                    case mr of
                      Nothing -> return ()
                      Just r -> putMVar (getPromise r) $ Left $ toException x
                  failFrom reqStore = fail' reqStore $ RequestFailed details errorUri
              case msgType of
                MsgTypeSubscribe   -> failFrom sessionSubscribeRequests
                MsgTypeUnsubscribe -> failFrom sessionUnsubscribeRequests
                MsgTypePublish     -> failFrom sessionPublishRequests
                MsgTypeRegister    -> failFrom sessionRegisterRequests
                MsgTypeUnregister  -> failFrom sessionUnregisterRequests
                MsgTypeCall        -> fail' sessionCallRequests $
                                      RpcError details errorUri args argskw
                _ -> throwIO $ ProtocolException $ "Unexpected error message: " ++ show msg
              go

            Published reqId pubId -> do
              mpr <- extract (sessionPublishRequests session) reqId
              case mpr of
                Nothing -> return ()
                Just pr -> putMVar (publishPromise pr) $ Right pubId
              go

            Subscribed reqId subId -> do
              msr <- extract (sessionSubscribeRequests session) reqId
              case msr of
                Nothing -> return ()
                Just sr -> do
                  let TicketStore mvsub = sessionSubscriptions session
                      sub = Subscription { subscriptionId = subId
                                         , subscriptionTopicUri = subscribeRequestTopicUri sr
                                         , subscriptionHandler = subscribeRequestHandler sr
                                         , subscriptionOptions = subscribeRequestOptions sr
                                         }
                  modifyMVar_ mvsub $ return . Ix.insert sub
                  putMVar (subscribePromise sr) $ Right sub
              go

            Unsubscribed reqId -> do
              mur <- extract (sessionUnsubscribeRequests session) reqId
              case mur of
                Nothing -> return ()
                Just ur -> do
                  let TicketStore mvsub = sessionSubscriptions session
                  modifyMVar_ mvsub $ return . Ix.deleteIx (unsubscribeRequestSubId ur)
                  putMVar (unsubscribePromise ur) $ Right True
              go

            Event subId pubId details args argskw -> do
              let TicketStore mvsub = sessionSubscriptions session
              subs <- readMVar mvsub
              case Ix.getOne $ subs Ix.@= subId of
                Nothing -> return ()
                Just sub -> subscriptionHandler sub args argskw details
              go

            Result reqId details args argskw -> do
              mreq <- extract (sessionCallRequests session) reqId
              case mreq of
                Nothing -> return ()
                Just req -> putMVar (callPromise req) $ Right (args, argskw)
              go

            Registered reqId regId -> do
              mrr <- extract (sessionRegisterRequests session) reqId
              case mrr of
                Nothing -> return ()
                Just rr -> do
                  let TicketStore mvreg = sessionRegistrations session
                      reg = Registration { registrationId = regId
                                         , registrationProcedureUri = registerRequestProcUri rr
                                         , registrationEndpoint = registerRequestEndpoint rr
                                         , registrationHandleAsync = registerRequestHandleAsync rr
                                         , registrationOptions = registerRequestOptions rr }
                  modifyMVar_ mvreg $ return . Ix.insert reg
                  putMVar (registerPromise rr) $ Right reg
              go

            Unregistered reqId -> do
              mur <- extract (sessionUnregisterRequests session) reqId
              case mur of
                Nothing -> return ()
                Just ur -> do
                  let TicketStore mvreg = sessionRegistrations session
                      regId = unregisterRequestSubId ur
                      promise = unregisterPromise ur
                  modifyMVar_ mvreg $ return . Ix.deleteIx regId
                  putMVar promise $ Right True
              go

            Invocation reqId regId details args argskw -> do
              let TicketStore mvreg = sessionRegistrations session
              regs <- readMVar mvreg
              case Ix.getOne $ regs Ix.@= regId of
                Nothing -> sendMessage conn $
                           defaultError MsgTypeInvocation reqId "wamp.error.no_such_registration"
                Just reg ->
                  let call = registrationEndpoint reg args argskw details
                      complete eres =
                        case eres of
                          Left x ->
                            sendMessage conn $
                            Error MsgTypeInvocation reqId (Details HM.empty)
                            "wamp.error.exception"
                            (Arguments $ V.singleton $ String $ T.pack $ show (x::SomeException))
                            (ArgumentsKw HM.empty)
                          Right (res,reskw) ->
                            sendMessage conn $ Yield reqId (Options HM.empty) res reskw
                  in if registrationHandleAsync reg
                     then void $ forkFinally call complete
                     else try call >>= complete
              go

            _ -> throwIO $ ProtocolException $ "Unexpected message: " ++ show msg

subscribe' :: Session -> TopicUri -> Options -> Handler -> IO (Result Subscription)
subscribe' session topicUri opts handler = do
  reqId <- sessionGenId session >>= return . ReqId
  m <- newEmptyMVar
  insert (sessionSubscribeRequests session) $ SubscribeRequest m reqId topicUri handler opts
  sendMessage (sessionConnection session) $ Subscribe reqId opts topicUri
  return m

unsubscribe' :: Session -> Subscription -> IO (Result Bool)
unsubscribe' session sub = do
  let subId = subscriptionId sub
  reqId <- sessionGenId session >>= return . ReqId
  m <- newEmptyMVar
  insert (sessionUnsubscribeRequests session) $ UnsubscribeRequest m reqId subId
  sendMessage (sessionConnection session) $ Unsubscribe reqId subId
  return m

publishAck' :: Session -> TopicUri -> Arguments -> ArgumentsKw -> Options -> IO (Result PubId)
publishAck' session topicUri args kwArgs (Options optionsDict) = do
  reqId <- sessionGenId session >>= return . ReqId
  res <- newEmptyMVar
  let opts = Options $ HM.insert "acknowledge" (Bool True) optionsDict
      pub = Publish reqId opts topicUri args kwArgs
      pubrq = PublishRequest res reqId
  insert (sessionPublishRequests session) pubrq
  sendMessage (sessionConnection session) pub
  return res

publish :: Session -> TopicUri -> Arguments -> ArgumentsKw -> Options -> IO ()
publish session topicUri args kwArgs opts = do
  reqId <- sessionGenId session >>= return . ReqId
  let pub = Publish reqId opts topicUri args kwArgs
  sendMessage (sessionConnection session) pub

register' :: Session -> ProcedureUri -> Endpoint -> Bool -> Options -> IO (Result Registration)
register' session procedureUri endpoint handleAsync opts = do
  reqId <- sessionGenId session >>= return . ReqId
  m <- newEmptyMVar
  insert (sessionRegisterRequests session) $
    RegisterRequest m reqId procedureUri endpoint handleAsync opts
  sendMessage (sessionConnection session) $ Register reqId opts procedureUri
  return m


unregister' :: Session -> Registration -> IO (Result Bool)
unregister' session reg = do
  let regId = registrationId reg
  reqId <- sessionGenId session >>= return . ReqId
  m <- newEmptyMVar
  insert (sessionUnregisterRequests session) $ UnregisterRequest m reqId regId
  sendMessage (sessionConnection session) $ Unregister reqId regId
  return m

call' :: Session -> ProcedureUri -> Arguments -> ArgumentsKw -> Options -> IO (Result CallResult)
call' session procedureUri args kwArgs opts = do
  reqId <- sessionGenId session >>= return . ReqId
  m <- newEmptyMVar
  insert (sessionCallRequests session) $ CallRequest m reqId
  sendMessage (sessionConnection session) $ Call reqId opts procedureUri args kwArgs
  return m

waitResult :: Result a -> IO a
waitResult resmv = do
  eres <- takeMVar resmv
  case eres of
    Right res -> return res
    Left x -> throwIO x

call a b c d e       = call' a b c d e       >>= waitResult
unregister a b       = unregister' a b       >>= waitResult
register a b c d e   = register' a b c d e   >>= waitResult
subscribe a b c d    = subscribe' a b c d    >>= waitResult
unsubscribe a b      = unsubscribe' a b      >>= waitResult
publishAck a b c d e = publishAck' a b c d e >>= waitResult

tryCall a b c d e       = call' a b c d e       >>= takeMVar
tryUnregister a b       = unregister' a b       >>= takeMVar
tryRegister a b c d e   = register' a b c d e   >>= takeMVar
trySubscribe a b c d    = subscribe' a b c d    >>= takeMVar
tryUnsubscribe a b      = unsubscribe' a b      >>= takeMVar
tryPublishAck a b c d e = publishAck' a b c d e >>= takeMVar


completeSessionClosed :: Result a -> IO ()
completeSessionClosed r = tryTakeMVar r >> putMVar r (Left $ toException $ SessionClosed)

finalizeOutstanding :: (Storeable a, HasPromise a b) => Store a -> IO ()
finalizeOutstanding stor = do
  x <- let Store s = stor in takeMVar s
  forConcurrently_ (Ix.toList x) $ completeSessionClosed . getPromise

finalizeSession :: Session -> IO ()
finalizeSession session =
  forConcurrently_ [finalizeOutstanding . sessionPublishRequests
                   ,finalizeOutstanding . sessionSubscribeRequests
                   ,finalizeOutstanding . sessionUnsubscribeRequests
                   ,finalizeOutstanding . sessionCallRequests
                   ,finalizeOutstanding . sessionRegisterRequests
                   ,finalizeOutstanding . sessionUnregisterRequests
                   ] ($ session)
  
genGlobalId :: IO ID
genGlobalId = R.randomRIO (0, 2^(53 :: Int))
