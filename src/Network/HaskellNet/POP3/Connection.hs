module Network.HaskellNet.POP3.Connection
    ( POP3Connection
    , HasPOP3Connection(..)
    , getBSStream
    , withBSStream
    , stream
    , newConnection
    , apopKey
    )
where

import Network.HaskellNet.BSStream

data POP3Connection m =
    POP3C { stream :: !(BSStream m)
          , apopKey :: !String -- ^ APOP key
          }

class HasPOP3Connection m where
  getPOP3Connection :: m (POP3Connection m)

getBSStream :: (Functor m,HasPOP3Connection m) => m (BSStream m)
getBSStream = stream <$> getPOP3Connection

withBSStream :: (Monad m,HasPOP3Connection m) => (BSStream m -> m a) -> m a
withBSStream action = do
  s <- getBSStream
  action s

newConnection :: BSStream m -> String -> POP3Connection m
newConnection = POP3C
