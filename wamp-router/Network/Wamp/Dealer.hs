{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      : Network.Wamp.Dealer
-- Description : Delaer
-- Copyright   : (c) Maciej Kazulak, 2015
-- License     : MIT
-- Maintainer  : kazulakm@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- WAMP Dealer.
--
module Network.Wamp.Dealer
  ( Registration (..)
  , RegistrationStore
  , InvocationInfo (..)
  , InvocationStore
  , CalleeSessId (..)

  , newRegistrationStore
  , insertRegistration
  , lookupRegistration
  , lookupRegistrationByProcedureUri
  , deleteRegistration
  , deleteRegistrationBySessId
  , countRegistration

  , newInvocationStore
  , insertInvocation
  , lookupInvocation
  , takeInvocation
  , deleteInvocation
  , deleteInvocationByCalleeSessId
  , deleteInvocationByCallerSessId
  , countInvocation
  )
where

import Prelude hiding (null)
import Control.Concurrent.MVar
import Data.IxSet
import Data.Typeable

import Network.Wamp.Connection
import Network.Wamp.Types


-- | Remote procedure registration as seen by a @Dealer@
--
-- @Dealer@ stores one 'Registration' for each remote procedure registered by a @Callee@.
data Registration = Registration
  { registrationId            :: !RegId
  , registrationSessionId     :: !SessId        -- ^ @Callee@ session id
  , registrationProcedureUri  :: !ProcedureUri
  , registrationSession       :: !Session       -- ^ @Callee@ session
  }
  deriving (Eq, Typeable, Show)

instance Ord Registration where
  compare x y = compare (registrationId x) (registrationId y)

instance Indexable Registration where
  empty = ixSet
    [ ixFun $ \r -> [registrationId r]
    , ixFun $ \r -> [registrationSessionId r]
    , ixFun $ \r -> [registrationProcedureUri r]
    ]

-- | Current remote procedure registrations known to a @Dealer@
newtype RegistrationStore = RegistrationStore (MVar (IxSet Registration))


-- | Create a new 'RegistrationStore'
newRegistrationStore :: IO RegistrationStore
newRegistrationStore = do
  m <- newMVar empty
  return $ RegistrationStore m

-- | Insert a 'Registration' into a 'RegistrationStore'
--
-- Returns True if the operation was successful or False if a 'Network.Wamp.Types.ProcedureUri'
-- is already registered in which case the 'RegistrationStore' is left unmodified.
insertRegistration :: RegistrationStore -> Registration -> IO Bool
insertRegistration (RegistrationStore m) reg = do
  store <- takeMVar m
  let notRegistered = null $ store @= (registrationProcedureUri reg)
  case notRegistered of
    False  -> do
      putMVar m store
      return False
    True -> do
      putMVar m $ insert reg store
      return True

-- | Lookup a 'Registration' by 'Network.Wamp.Types.RegId'
lookupRegistration :: RegistrationStore -> RegId -> IO (Maybe Registration)
lookupRegistration (RegistrationStore m) regId = do
  store <- readMVar m
  return $ getOne $ store @= regId

-- | Lookup a 'Registration' by 'Network.Wamp.Types.ProcedureUri'
lookupRegistrationByProcedureUri :: RegistrationStore -> ProcedureUri -> IO (Maybe Registration)
lookupRegistrationByProcedureUri (RegistrationStore m) procUri = do
  store <- readMVar m
  return $ getOne $ store @= procUri

-- | Delete 'Registration' by 'Network.Wamp.Types.RegId'
--
-- Returns True if the operation was successful or False if no such registration exists
-- in which case 'RegistrationStore' is left unmodified.
deleteRegistration :: RegistrationStore -> RegId -> IO Bool
deleteRegistration (RegistrationStore m) regId = do
  store <- takeMVar m
  case getOne $ store @= regId of
    Nothing -> do
      putMVar m store
      return False
    Just r  -> do
      putMVar m $ delete r store
      return True

-- | Delete all 'Registration's by @Callee@ session id.
--
-- Used for cleanup after @Callee@ session ends.
deleteRegistrationBySessId :: RegistrationStore -> SessId -> IO ()
deleteRegistrationBySessId (RegistrationStore m) sessId = do
  store <- takeMVar m
  let ds = toList $ store @= sessId
  putMVar m $ foldr delete store ds

-- | Return current registration count
countRegistration :: RegistrationStore -> IO Int
countRegistration (RegistrationStore m) = do
  store <- readMVar m
  return $ size store



-- | Wrapper for Indexable
newtype CalleeSessId = CalleeSessId SessId
  deriving (Eq, Ord, Show, Typeable)


-- | Remote procedure invocation as seen by a @Dealer@
--
-- An 'InvocationInfo' is stored by a @Dealer@ for each @Invocation@ sent.
-- Upon receiving a @Yield@ from the @Callee@ it must have this information
-- to know the @Caller@ session and the original @Call@ 'Network.Wamp.Types.ReqId'.
data InvocationInfo = InvocationInfo
  { invocationReqId             :: !ReqId          -- ^ @Dealer@ invocation request id
  , invocationCalleeSessionId   :: !CalleeSessId   -- ^ @Callee@ session id
  , invocationCallerSessionId   :: !SessId         -- ^ @Caller@ session id
  , invocationCallerReqId       :: !ReqId          -- ^ ReqId from the @Call@ message
  , invocationCallerSession     :: !Session        -- ^ @Caller@ session
  }
  deriving (Eq, Typeable, Show)

instance Ord InvocationInfo where
  compare x y = compare (invocationReqId x) (invocationReqId y)

instance Indexable InvocationInfo where
  empty = ixSet
    [ ixFun $ \r -> [invocationReqId r]
    , ixFun $ \r -> [invocationCalleeSessionId r]
    , ixFun $ \r -> [invocationCallerSessionId r]
    ]

-- | Current invocations known to a @Dealer@
newtype InvocationStore = InvocationStore (MVar (IxSet InvocationInfo))


-- | Create a new 'InvocationStore'
newInvocationStore :: IO InvocationStore
newInvocationStore = do
  m <- newMVar empty
  return $ InvocationStore m

-- | Insert an 'InvocationInfo' into an 'InvocationStore'
insertInvocation :: InvocationStore -> InvocationInfo -> IO ()
insertInvocation (InvocationStore m) inv = do
  store <- takeMVar m
  putMVar m $ insert inv store

-- | Lookup an 'InvocationInfo' by 'Network.Wamp.Types.ReqId'
lookupInvocation :: InvocationStore -> ReqId -> IO (Maybe InvocationInfo)
lookupInvocation (InvocationStore m) invReqId = do
  store <- readMVar m
  return $ getOne $ store @= invReqId

-- | Atomic lookup and delete.
takeInvocation :: InvocationStore -> ReqId -> IO (Maybe InvocationInfo)
takeInvocation (InvocationStore m) invReqId = do
  store <- takeMVar m
  let mInv = getOne $ store @= invReqId
  case mInv of
    Nothing  -> putMVar m store
    Just inv -> putMVar m $ delete inv store
  return mInv

-- | Delete an 'InvocationInfo' by 'Network.Wamp.Types.ReqId'
deleteInvocation :: InvocationStore -> ReqId -> IO ()
deleteInvocation (InvocationStore m) invReqId = do
  store <- takeMVar m
  putMVar m $ deleteIx invReqId store

-- | Delete all 'InvocationInfo's by @Caller@ session id.
--
-- Used for cleanup after @Callee@ session ends.
deleteInvocationByCalleeSessId :: InvocationStore -> SessId -> IO ()
deleteInvocationByCalleeSessId (InvocationStore m) sessId = do
  store <- takeMVar m
  let ds = toList $ store @= sessId
  putMVar m $ foldr delete store ds

-- | Delete all 'InvocationInfo's by @Caller@ session id.
--
-- Used for cleanup after @Caller@ session ends.
deleteInvocationByCallerSessId :: InvocationStore -> SessId -> IO ()
deleteInvocationByCallerSessId (InvocationStore m) sessId = do
  store <- takeMVar m
  let ds = toList $ store @= (CalleeSessId sessId)
  putMVar m $ foldr delete store ds

-- | Return current invocation count
countInvocation :: InvocationStore -> IO Int
countInvocation (InvocationStore m) = do
  store <- readMVar m
  return $ size store
