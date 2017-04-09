module Network.HaskellNet.IMAP.Connection
    ( IMAPConnection
    , HasIMAPConnection(..)
    , getBSStream
    , withBSStream
    , withNextCommandNum
    , setMailboxInfo
    , modifyMailboxInfo
    , newConnection
    , mailbox
    , exists
    , recent
    , flags
    , permanentFlags
    , isWritable
    , isFlagWritable
    , uidNext
    , uidValidity
    , stream
    )
where

import Data.IORef
    ( IORef
    , newIORef
    , readIORef
    , writeIORef
    , modifyIORef
    )
import Control.Applicative
    ( (<$>)
    , (<*>)
    )

import Network.HaskellNet.BSStream
import Network.HaskellNet.IMAP.Types
    ( MailboxInfo(..)
    , emptyMboxInfo
    , MailboxName
    , Flag
    , UID
    )

import Control.Monad.IO.Class

data IMAPConnection m =
    IMAPC { stream :: BSStream m
          , mboxInfo :: IORef MailboxInfo
          , nextCommandNum :: IORef Int
          }

class HasIMAPConnection m where
  getIMAPConnection :: m (IMAPConnection m)

getBSStream :: (Functor m,HasIMAPConnection m) => m (BSStream m)
getBSStream = stream <$> getIMAPConnection

withBSStream :: (Monad m,HasIMAPConnection m) => (BSStream m -> m a) -> m a
withBSStream action = do
  conn <- getIMAPConnection
  action (stream conn)

newConnection :: MonadIO m => BSStream m -> m (IMAPConnection m)
newConnection s = IMAPC s <$> liftIO (newIORef emptyMboxInfo) <*> liftIO (newIORef 0)

getMailboxInfo :: (MonadIO m,HasIMAPConnection m) => m MailboxInfo
getMailboxInfo = do
  conn <- getIMAPConnection
  liftIO $ readIORef $ mboxInfo conn

mailbox :: (MonadIO m,HasIMAPConnection m) => m MailboxName
mailbox = _mailbox <$> getMailboxInfo

exists :: (MonadIO m,HasIMAPConnection m) => m Integer
exists = _exists <$> getMailboxInfo 

recent :: (MonadIO m,HasIMAPConnection m) => m Integer
recent = _recent <$> getMailboxInfo 

flags :: (MonadIO m,HasIMAPConnection m) => m [Flag]
flags = _flags <$> getMailboxInfo 

permanentFlags :: (MonadIO m,HasIMAPConnection m) => m [Flag]
permanentFlags = _permanentFlags <$> getMailboxInfo 

isWritable :: (MonadIO m,HasIMAPConnection m) => m Bool
isWritable = _isWritable <$> getMailboxInfo 

isFlagWritable :: (MonadIO m,HasIMAPConnection m) => m Bool
isFlagWritable = _isFlagWritable <$> getMailboxInfo 

uidNext :: (MonadIO m,HasIMAPConnection m) => m UID
uidNext = _uidNext <$> getMailboxInfo 

uidValidity :: (MonadIO m,HasIMAPConnection m) => m UID
uidValidity = _uidValidity <$> getMailboxInfo 

withNextCommandNum :: (MonadIO m,HasIMAPConnection m) => (Int -> m a) -> m (a, Int)
withNextCommandNum act = do
  conn <- getIMAPConnection
  let ref = nextCommandNum conn
  num <- liftIO $ readIORef ref
  result <- act num
  liftIO $ modifyIORef ref (+1)
  return (result, num)

setMailboxInfo :: (MonadIO m,HasIMAPConnection m) => MailboxInfo -> m ()
setMailboxInfo mboxinfo = do
  conn <- getIMAPConnection
  liftIO $ writeIORef (mboxInfo conn) mboxinfo

modifyMailboxInfo :: (MonadIO m,HasIMAPConnection m) => (MailboxInfo -> MailboxInfo) -> m ()
modifyMailboxInfo f = do
  conn <- getIMAPConnection
  liftIO $ modifyIORef (mboxInfo conn) f
