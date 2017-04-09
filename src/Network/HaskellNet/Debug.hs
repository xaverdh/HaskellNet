module Network.HaskellNet.Debug
    ( debugStream
    )
    where

import Network.HaskellNet.BSStream
import qualified Data.ByteString.Char8 as BS
import System.IO
import Control.Monad.IO.Class

debugStream :: MonadIO m => BSStream m -> BSStream m
debugStream inner =
    inner { bsGetLine = debugBsGetLine inner
          , bsGet = debugBsGet inner
          , bsPut = debugBsPut inner
          }

debugBsGetLine :: MonadIO m => BSStream m -> m BS.ByteString
debugBsGetLine s = do
  liftIO $ do
    hPutStr stderr "reading with bsGetLine..."
    hFlush stderr
  l <- bsGetLine s
  liftIO $ BS.hPutStrLn stderr l
  return l

debugBsGet :: MonadIO m => BSStream m -> Int -> m BS.ByteString
debugBsGet s len = do
  liftIO $ do
    hPutStr stderr $ "reading with bsGet "++show len++"..."
    hFlush stderr
  chunk <- bsGet s len
  liftIO $ BS.hPutStrLn stderr chunk
  return chunk

debugBsPut :: MonadIO m => BSStream m -> BS.ByteString -> m ()
debugBsPut s str = do
  liftIO $ do
    hPutStr stderr "putting with bsPut ("
    BS.hPutStrLn stderr str
    hPutStr stderr (")...")
    hFlush stderr
  bsPut s str
  bsFlush s
  liftIO $ hPutStrLn stderr "done"
  return ()
