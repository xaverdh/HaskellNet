{-# LANGUAGE ScopedTypeVariables #-}
{- |

This module provides functions for working with the SMTP protocol in the client side,
including /opening/ and /closing/ connections, /sending commands/ to the server,
/authenticate/ and /sending mails/.

Here's a basic usage example:

>
> import Network.HaskellNet.SMTP
> import Network.HaskellNet.Auth
> import qualified Data.Text.Lazy as T
>
> main = doSMTP "your.smtp.server.com" $ \conn ->
>    authSucceed <- authenticate PLAIN "username" "password" conn
>    if authSucceed
>        then sendPlainTextMail "receiver@server.com" "sender@server.com" "subject" (T.pack "Hello! This is the mail body!") conn
>        else print "Authentication failed."

Notes for the above example:

   * First the 'SMTPConnection' is opened with the 'doSMTP' function.
     The connection should also be established with functions such as 'connectSMTP',
     'connectSMTPPort' and 'doSMTPPort'.
     With the @doSMTP*@ functions the connection is opened, then executed an action
     with it and then closed automatically.
     If the connection is opened with the @connectSMTP*@ functions you may want to
     close it with the 'closeSMTP' function after using it.
     It is also possible to create a 'SMTPConnection' from an already opened connection
     stream ('BSStream') using the 'connectStream' or 'doSMTPStream' functions.

     /NOTE:/ For /SSL\/TLS/ support you may establish the connection using
             the functions (such as @connectSMTPSSL@) provided in the
             @Network.HaskellNet.SMTP.SSL@ module of the
             <http://hackage.haskell.org/package/HaskellNet-SSL HaskellNet-SSL>
             package.

   * The 'authenticate' function authenticates to the server with the specified 'AuthType'.
     'PLAIN', 'LOGIN' and 'CRAM_MD5' 'AuthType's are available. It returns a 'Bool'
     indicating either the authentication succeed or not.


   * To send a mail you can use 'sendPlainTextMail' for plain text mail, or 'sendMimeMail'
     for mime mail.
-}
module Network.HaskellNet.SMTP
    ( -- * Types
      Command(..)
    , Response(..)
    , AuthType(..)
    , SMTPConnection
      -- * Establishing Connection
    , connectSMTPPort
    , connectSMTP
    , connectStream
      -- * Operation to a Connection
    , sendCommand
    , closeSMTP
      -- * Other Useful Operations
    , authenticate
    -- , doSMTPPort
    -- , doSMTP
    -- , doSMTPStream
    , sendMail
    , sendPlainTextMail
    , sendMimeMail
    , sendMimeMail'
    , sendMimeMail2
    )
    where

import Network.HaskellNet.BSStream
import Network.HaskellNet.SMTP.Types
import Network.HaskellNet.SMTP.Internal

import Data.ByteString (ByteString)
import qualified Network

-- import Control.Exception
import Control.Monad (unless, when)
import Control.Monad.IO.Class

import Network.HaskellNet.Auth

import Network.Mail.Mime
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T

class HasSMTPConnection m where
  getSMTPConnection :: m (SMTPConnection m)


-- | connecting SMTP server with the specified name and port number.
connectSMTPPort :: MonadIO m
                => String     -- ^ name of the server
                -> Network.PortNumber -- ^ port number
                -> m (SMTPConnection m)
connectSMTPPort hostname port = do
    h <- liftIO $ Network.connectTo hostname (Network.PortNumber port)
    connectStream (handleToStream h)

-- | connecting SMTP server with the specified name and port 25.
connectSMTP :: MonadIO m
            => String     -- ^ name of the server
            -> m (SMTPConnection m)
connectSMTP = flip connectSMTPPort 25

-- | send a method to a server
sendCommand :: (Monad m,HasSMTPConnection m)
            => Command
            -> m (ReplyCode, ByteString)
sendCommand cmd = getSMTPConnection
  >>= flip sendCommandInternal cmd

-- | close the connection.  This function send the QUIT method, so you
-- do not have to QUIT method explicitly.
closeSMTP :: (Monad m,HasSMTPConnection m) => m ()
closeSMTP = getSMTPConnection >>= closeSMTPInternal

{- |
This function will return 'True' if the authentication succeeds.
Here's an example of sending a mail with a server that requires
authentication:

>    authSucceed <- authenticate PLAIN "username" "password"
>    if authSucceed
>        then sendPlainTextMail "receiver@server.com" "sender@server.com" "subject" (T.pack "Hello!")
>        else print "Authentication failed."
-}
authenticate :: (Monad m,HasSMTPConnection m) => AuthType -> UserName -> Password -> m Bool
authenticate at username password = do
    (code, _) <- sendCommand $ AUTH at username password
    return (code == 235)

{-
-- | doSMTPPort open a connection, and do an IO action with the
-- connection, and then close it.
doSMTPPort :: String -> PortNumber -> (SMTPConnection m -> m a) -> IO a
doSMTPPort host port =
    bracket (connectSMTPPort host port) closeSMTPInternal

-- | doSMTP is similar to doSMTPPort, except that it does not require
-- port number but connects to the server with port 25.
doSMTP :: String -> (SMTPConnection m -> m a) -> IO a
doSMTP host = doSMTPPort host 25

-- | doSMTPStream is similar to doSMTPPort, except that its argument
-- is a Stream data instead of hostname and port number.
doSMTPStream :: BSStream m -> (SMTPConnection m -> m a) -> IO a
doSMTPStream s = bracket (connectStream s) closeSMTPInternal
-}

-- | sending a mail to a server. This is achieved by sendMessage.  If
-- something is wrong, it raises an IOexception.
sendMail :: (Monad m,HasSMTPConnection m)
         => String     -- ^ sender mail
         -> [String]   -- ^ receivers
         -> ByteString -- ^ data
         -> m ()
sendMail sender receivers dat = do
    sendAndCheck (MAIL sender)
    mapM_ (sendAndCheck . RCPT) receivers
    sendAndCheck (DATA dat)
    return ()
  where
    -- Try the command once and @fail@ if the response isn't 250.
    sendAndCheck cmd = do
      conn <- getSMTPConnection
      tryCommand conn cmd 1 250

-- | Send a plain text mail.
sendPlainTextMail :: (MonadIO m,HasSMTPConnection m)
                  => String  -- ^ receiver
                  -> String  -- ^ sender
                  -> String  -- ^ subject
                  -> LT.Text -- ^ body
                  -> m ()
sendPlainTextMail to from subject body = do
    renderedMail <- liftIO $ renderMail' myMail
    sendMail from [to] (lazyToStrict renderedMail)
    where
        myMail = simpleMail' (address to) (address from) (T.pack subject) body
        address = Address Nothing . T.pack

-- | Send a mime mail. The attachments are included with the file path.
sendMimeMail :: (MonadIO m,HasSMTPConnection m)
             => String               -- ^ receiver
             -> String               -- ^ sender
             -> String               -- ^ subject
             -> LT.Text              -- ^ plain text body
             -> LT.Text              -- ^ html body
             -> [(T.Text, FilePath)] -- ^ attachments: [(content_type, path)]
             -> m ()
sendMimeMail to from subject plainBody htmlBody attachments = do
  renderedMail <- liftIO $ do
      myMail <- simpleMail (address to) (address from) (T.pack subject)
                plainBody htmlBody attachments
      renderMail' myMail
  sendMail from [to] (lazyToStrict renderedMail)
  where
    address = Address Nothing . T.pack

-- | Send a mime mail. The attachments are included with in-memory 'ByteString'.
sendMimeMail' :: (MonadIO m,HasSMTPConnection m)
              => String                         -- ^ receiver
              -> String                         -- ^ sender
              -> String                         -- ^ subject
              -> LT.Text                        -- ^ plain text body
              -> LT.Text                        -- ^ html body
              -> [(T.Text, T.Text, B.ByteString)] -- ^ attachments: [(content_type, file_name, content)]
              -> m ()
sendMimeMail' to from subject plainBody htmlBody attachments = do
  let myMail = simpleMailInMemory (address to) (address from) (T.pack subject)
                                  plainBody htmlBody attachments
  sendMimeMail2 myMail
  where
    address = Address Nothing . T.pack

sendMimeMail2 :: (MonadIO m,HasSMTPConnection m) => Mail -> m ()
sendMimeMail2 mail = do
    let (Address _ from) = mailFrom mail
        recps = map (T.unpack . addressEmail)
                     $ (mailTo mail ++ mailCc mail ++ mailBcc mail)
    when (null recps) $ fail "no receiver specified."
    renderedMail <- liftIO $ renderMail' $ mail { mailBcc = [] }
    sendMail (T.unpack from) recps (lazyToStrict renderedMail)

-- haskellNet uses strict bytestrings
-- TODO: look at making haskellnet lazy
lazyToStrict :: B.ByteString -> S.ByteString
lazyToStrict = B.toStrict
