module Network.HaskellNet.SMTP.Types
    ( Command(..)
    , Response(..)
    , SMTPConnection(..)
    , ReplyCode
    )
where

import Network.HaskellNet.BSStream
import Network.HaskellNet.Auth
import Data.ByteString (ByteString)


-- The response field seems to be unused. It's saved at one place, but never
-- retrieved.
data SMTPConnection m = SMTPC
  { bsstream :: !(BSStream m), _response :: ![ByteString] }

data Command = HELO String
             | EHLO String
             | MAIL String
             | RCPT String
             | DATA ByteString
             | EXPN String
             | VRFY String
             | HELP String
             | AUTH AuthType UserName Password
             | NOOP
             | RSET
             | QUIT
               deriving (Show, Eq)

type ReplyCode = Int

data Response = Ok
              | SystemStatus
              | HelpMessage
              | ServiceReady
              | ServiceClosing
              | UserNotLocal
              | CannotVerify
              | StartMailInput
              | ServiceNotAvailable
              | MailboxUnavailable
              | ErrorInProcessing
              | InsufficientSystemStorage
              | SyntaxError
              | ParameterError
              | CommandNotImplemented
              | BadSequence
              | ParameterNotImplemented
              | MailboxUnavailableError
              | UserNotLocalError
              | ExceededStorage
              | MailboxNotAllowed
              | TransactionFailed
                deriving (Show, Eq)


