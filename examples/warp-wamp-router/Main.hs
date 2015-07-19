{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- see for wai-websockets example:
-- https://github.com/yesodweb/wai/blob/master/wai-websockets/server.lhs

import           Control.Concurrent.Async       (async, link)
import           Control.Exception              (throwIO, catch, finally)
import           Control.Monad                  (forever)
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static
import qualified Network.WebSockets             as WS
import           Data.FileEmbed                 (embedDir)

import           Data.Aeson                     (Value (Bool))
import qualified Data.HashMap.Strict            as HM

import           Network.Wamp

port :: Int
port = 3000

main :: IO ()
main = do
  let realm = "realm1"
  r <- defaultRouter realm
  let realms = HM.singleton realm r

  putStrLn $ "Listening on http://localhost:" ++ show port
  Warp.runSettings
    (Warp.setPort port Warp.defaultSettings)
    $ WaiWS.websocketsOr WS.defaultConnectionOptions (application realms) staticApp


staticApp :: Wai.Application
staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "static")


application :: RealmMap -> WS.ServerApp
application realms pc = do
  putStrLn "\nNew connection"
  conn <- acceptWsRequest pc

  forever $ do
    (session, router) <- establishSession realms conn

    messageLoop router session
      `catch` (\e ->
        putStrLn $ show (e :: WampException))
      `finally` (
        cleanupSession router session)


messageLoop :: Router -> Session -> IO ()
messageLoop router session = do
  -- establishSession does not modify any state by convention, registering the session here we can be sure
  -- it will be cleaned up even if the client drops the connection because message loop is wrapped in finally
  insertSession (routerSessions router) session
  putStrLn $ "Session established: " ++ show (routerRealm router) ++ " " ++ show (sessionId session)

  forever $ do
    m <- receiveMessage (sessionConnection session)
    -- link ensures we get any exceptions thrown by messageHandler
    (async $ messageHandler router session m) >>= link


messageHandler :: Router -> Session -> Message -> IO ()
messageHandler router session m = do
  -- current connection and session identifier
  let conn = sessionConnection session
  let sid  = sessionId session

  -- TODO: verify syntax for all received URIs
  
  case m of
    Goodbye _ _ -> do
      sendMessage conn $ Goodbye (Details dict) "wamp.close.goodbye_and_out"
      throwIO SessionClosed

    --
    -- Broker - PubSub
    --
    Subscribe reqId _ topicUri -> do
      -- In case of receiving a SUBSCRIBE message from the same Subscriber and to already subscribed topic,
      -- Broker should answer with SUBSCRIBED message, containing the existing Subscription|id.
      -- 
      -- For discussion see: https://github.com/tavendo/WAMP/issues/142
      mSub <- lookupSubscriptionByTopicSubscriber (routerSubscriptions router) topicUri sid
      case mSub of
        Nothing -> do
          -- TODO: use a single SubId per TopicUri so its possible to serialize the message once per Event
          -- for all Subscribers
          subId <- routerGenRouterId router >>= return . SubId
          insertSubscription (routerSubscriptions router) (Subscription subId (sessionId session) topicUri session)
          sendMessage conn $ Subscribed reqId subId
        Just sub -> sendMessage conn $ Subscribed reqId (subscriptionId sub)

    Unsubscribe reqId subId -> do
      mSub <- lookupSubscription (routerSubscriptions router) subId
      case mSub of
        Nothing -> sendMessage conn $ defaultError MsgTypeUnsubscribe reqId "wamp.error.no_such_subscription"
        Just (Subscription _ sessId _ _) -> do
          -- client can only unsubscribe its own subscriptions
          case (sessionId session) == (sessId) of
            False -> sendMessage conn $ defaultError MsgTypeUnsubscribe reqId "wamp.error.no_such_subscription"
            True  -> do
              deleteSubscription (routerSubscriptions router) subId
              sendMessage conn $ Unsubscribed reqId

    Publish reqId (Options opts) _ _ _ -> do
      pubId <- routerGenGlobalId router >>= return . PubId
      -- In the "Publishing and Events" section spec has a timeline that shows
      -- Published being sent to the Publisher before any Events are sent to
      -- Subscribers, so Published is really just a way to inform that
      -- authorization was successful and the Publish message was accepted
      -- for processing.
      --
      -- Moreover Published must only be sent when opts.acknowledge == True.
      case HM.lookup "acknowledge" opts of
        Just (Bool True) -> sendMessage conn $ Published reqId pubId
        _                -> return ()

      -- TODO: Note that the Publisher of an event will never receive the published
      -- event even if the Publisher is also a Subscriber of the topic published to.
      (async $ notifySubscribers router pubId m) >>= link

    --
    -- Dealer - RPC
    --
    Register reqId _ procUri -> do
      regId <- routerGenRouterId router >>= return . RegId
      res <- insertRegistration (routerRegistrations router) (Registration regId (sessionId session) procUri session)
      case res of
        True  -> sendMessage conn $ Registered reqId regId
        False -> sendMessage conn $ defaultError MsgTypeRegister reqId "wamp.error.procedure_already_exists"

    Unregister reqId regId -> do
      res <- deleteRegistration (routerRegistrations router) regId
      case res of
        True  -> sendMessage conn $ Unregistered reqId
        False -> sendMessage conn $ defaultError MsgTypeUnregister reqId "wamp.error.no_such_registration"

    Call reqId _ procUri args kwArgs -> do
      ms <- lookupRegistrationByProcedureUri (routerRegistrations router) procUri
      case ms of
        Nothing -> sendMessage conn $ defaultError MsgTypeCall reqId "wamp.error.no_such_procedure"
        Just (Registration regId _ _ calleeSession) -> do
          invId <- routerGenRouterId router >>= return . ReqId
          insertInvocation (routerInvocations router) (InvocationInfo invId (CalleeSessId $ sessionId calleeSession) (sessionId session) reqId session)
          sendMessage (sessionConnection calleeSession) $ Invocation invId regId (Details dict) args kwArgs

    Yield reqId _ args kwArgs -> do
      mInv <- takeInvocation (routerInvocations router) reqId
      case mInv of
        Nothing -> putStrLn $ "Got a Yield that does not match any Invocations. Its possible Caller session ended. Ignoring."
        Just (InvocationInfo _ _ _ callReqId callerSession) -> do
          sendMessage (sessionConnection callerSession) $ Result callReqId (Details dict) args kwArgs

    -- the only way a Router can receive an Error is if a Callee is unable to process an Invocation
    -- in which case we just relay it to the Caller
    Error MsgTypeInvocation reqId details errorUri args kwArgs -> do
      mInv <- takeInvocation (routerInvocations router) reqId
      case mInv of
        Nothing -> putStrLn $ "Got an Invocation Error that does not match any Invocations. Its possible Caller session ended. Ignoring."
        Just (InvocationInfo _ _ _ callReqId callerSession) -> do
          sendMessage (sessionConnection callerSession) $ Error MsgTypeCall callReqId details errorUri args kwArgs

    --
    -- Anything else is... unexpected.
    --
    _ -> throwIO $ ProtocolException $ "Unexpected message: " ++ show m
