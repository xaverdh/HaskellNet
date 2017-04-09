{-# LANGUAGE ScopedTypeVariables #-}
module Network.HaskellNet.SMTP.Internal
    ( 
      tryCommand 
    , connectStream
    , parseResponse
    , sendCommandInternal
    , closeSMTPInternal
    )
where

import Network.HaskellNet.BSStream
import Network.HaskellNet.SMTP.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Network.BSD (getHostName)

import Control.Monad (unless)
import Control.Monad.IO.Class

import Data.Char (isDigit)

import Network.HaskellNet.Auth


tryCommand :: Monad m
           => SMTPConnection m
           -> Command
           -> Int
           -> ReplyCode
           -> m ByteString
tryCommand conn cmd tries expectedReply = do
  (code, msg) <- sendCommandInternal conn cmd
  case () of
    _ | code == expectedReply   -> return msg
    _ | tries > 1               ->
          tryCommand conn cmd (tries - 1) expectedReply
      | otherwise               -> do
          bsClose (bsstream conn)
          fail $ "cannot execute command " ++ show cmd ++
                 ", expected reply code " ++ show expectedReply ++
                 ", but received " ++ show code ++ " " ++ BS.unpack msg

-- create SMTPConnection from already connected Stream
connectStream :: MonadIO m => BSStream m -> m (SMTPConnection m)
connectStream st =
    do (code1, _) <- parseResponse st
       unless (code1 == 220) $
              do bsClose st
                 fail "cannot connect to the server"
       senderHost <- liftIO getHostName
       msg <- tryCommand (SMTPC st []) (EHLO senderHost) 3 250
       return (SMTPC st (tail $ BS.lines msg))

parseResponse :: Monad m => BSStream m -> m (ReplyCode, ByteString)
parseResponse st =
    do (code, bdy) <- readLines
       return (read $ BS.unpack code, BS.unlines bdy)
    where readLines =
              do l <- bsGetLine st
                 let (c, bdy) = BS.span isDigit l
                 if not (BS.null bdy) && BS.head bdy == '-'
                    then do (c2, ls) <- readLines
                            return (c2, BS.tail bdy:ls)
                    else return (c, [BS.tail bdy])


-- send a method to a server
sendCommandInternal :: Monad m
                    => SMTPConnection m
                    -> Command
                    -> m (ReplyCode, ByteString)
sendCommandInternal (SMTPC conn _) (DATA dat) =
    do bsPutCrLf conn $ BS.pack "DATA"
       (code, _) <- parseResponse conn
       unless (code == 354) $ fail "this server cannot accept any data."
       mapM_ (sendLine . stripCR) $ BS.lines dat ++ [BS.pack "."]
       parseResponse conn
    where sendLine = bsPutCrLf conn
          stripCR bs = case BS.unsnoc bs of
                         Just (line, '\r') -> line
                         _                 -> bs
sendCommandInternal (SMTPC conn _) (AUTH LOGIN username password) =
    do bsPutCrLf conn command
       (_, _) <- parseResponse conn
       bsPutCrLf conn $ BS.pack userB64
       (_, _) <- parseResponse conn
       bsPutCrLf conn $ BS.pack passB64
       parseResponse conn
    where command = BS.pack "AUTH LOGIN"
          (userB64, passB64) = login username password
sendCommandInternal (SMTPC conn _) (AUTH at username password) =
    do bsPutCrLf conn command
       (code, msg) <- parseResponse conn
       unless (code == 334) $ fail "authentication failed."
       bsPutCrLf conn $ BS.pack $ auth at (BS.unpack msg) username password
       parseResponse conn
    where command = BS.pack $ unwords ["AUTH", show at]
sendCommandInternal (SMTPC conn _) meth =
    do bsPutCrLf conn $ BS.pack command
       parseResponse conn
    where command = case meth of
                      (HELO param) -> "HELO " ++ param
                      (EHLO param) -> "EHLO " ++ param
                      (MAIL param) -> "MAIL FROM:<" ++ param ++ ">"
                      (RCPT param) -> "RCPT TO:<" ++ param ++ ">"
                      (EXPN param) -> "EXPN " ++ param
                      (VRFY param) -> "VRFY " ++ param
                      (HELP msg)   -> if null msg
                                        then "HELP\r\n"
                                        else "HELP " ++ msg
                      NOOP         -> "NOOP"
                      RSET         -> "RSET"
                      QUIT         -> "QUIT"
                      (DATA _)     ->
                          error "BUG: DATA pattern should be matched by sendCommandInternal patterns"
                      (AUTH {})     ->
                          error "BUG: AUTH pattern should be matched by sendCommandInternal patterns"


-- | close the connection.  This function send the QUIT method, so you
-- do not have to QUIT method explicitly.
closeSMTPInternal :: SMTPConnection m -> m ()
closeSMTPInternal (SMTPC conn _) = bsClose conn

{-
I must be being stupid here

I can't seem to be able to catch the exception arising from the
connection already being closed this would be the correct way to do it
but instead we're being naughty above by just closes the connection
without first sending QUIT

closeSMTPInternal c@(SMTPC conn _) =
    do sendCommand c QUIT
       bsClose conn `catch` \(_ :: IOException) -> return ()
-}


crlf :: BS.ByteString
crlf = BS.pack "\r\n"

bsPutCrLf :: Monad m => BSStream m -> ByteString -> m ()
bsPutCrLf h s = bsPut h s >> bsPut h crlf >> bsFlush h
