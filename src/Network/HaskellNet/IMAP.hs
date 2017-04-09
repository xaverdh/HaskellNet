module Network.HaskellNet.IMAP
    ( connectIMAP, connectIMAPPort, connectStream
      -- * IMAP commands
      -- ** any state commands
    , noop, capability, logout
      -- ** not authenticated state commands
    , login, authenticate
      -- ** autenticated state commands
    , select, examine, create, delete, rename
    , subscribe, unsubscribe
    , list, lsub, status, append
      -- ** selected state commands
    , check, close, expunge
    , search, store, copy
    , idle
      -- * fetch commands
    , fetch, fetchHeader, fetchSize, fetchHeaderFields, fetchHeaderFieldsNot
    , fetchFlags, fetchR, fetchByString, fetchByStringR
      -- * other types
    , Flag(..), Attribute(..), MailboxStatus(..)
    , SearchQuery(..), FlagsQuery(..)
    , A.AuthType(..)
    )
where

import Network
import Network.HaskellNet.BSStream
import Network.HaskellNet.IMAP.Connection
import Network.HaskellNet.IMAP.Types
import Network.HaskellNet.IMAP.Parsers
import qualified Network.HaskellNet.Auth as A

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.IO.Class

import System.Time

import Data.Maybe
import Data.List hiding (delete)
import Data.Char

import Text.Packrat.Parse (Result)

-- suffixed by `s'
data SearchQuery = ALLs
                 | FLAG Flag
                 | UNFLAG Flag
                 | BCCs String
                 | BEFOREs CalendarTime
                 | BODYs String
                 | CCs String
                 | FROMs String
                 | HEADERs String String
                 | LARGERs Integer
                 | NEWs
                 | NOTs SearchQuery
                 | OLDs
                 | ONs CalendarTime
                 | ORs SearchQuery SearchQuery
                 | SENTBEFOREs CalendarTime
                 | SENTONs CalendarTime
                 | SENTSINCEs CalendarTime
                 | SINCEs CalendarTime
                 | SMALLERs Integer
                 | SUBJECTs String
                 | TEXTs String
                 | TOs String
                 | UIDs [UID]


instance Show SearchQuery where
    showsPrec d q = showParen (d>app_prec) $ showString $ showQuery q
        where app_prec = 10
              showQuery ALLs            = "ALL"
              showQuery (FLAG f)        = showFlag f
              showQuery (UNFLAG f)      = "UN" ++ showFlag f
              showQuery (BCCs addr)     = "BCC " ++ addr
              showQuery (BEFOREs t)     = "BEFORE " ++ dateToStringIMAP t
              showQuery (BODYs s)       = "BODY " ++ s
              showQuery (CCs addr)      = "CC " ++ addr
              showQuery (FROMs addr)    = "FROM " ++ addr
              showQuery (HEADERs f v)   = "HEADER " ++ f ++ " " ++ v
              showQuery (LARGERs siz)   = "LARGER {" ++ show siz ++ "}"
              showQuery NEWs            = "NEW"
              showQuery (NOTs qry)      = "NOT " ++ show qry
              showQuery OLDs            = "OLD"
              showQuery (ONs t)         = "ON " ++ dateToStringIMAP t
              showQuery (ORs q1 q2)     = "OR " ++ show q1 ++ " " ++ show q2
              showQuery (SENTBEFOREs t) = "SENTBEFORE " ++ dateToStringIMAP t
              showQuery (SENTONs t)     = "SENTON " ++ dateToStringIMAP t
              showQuery (SENTSINCEs t)  = "SENTSINCE " ++ dateToStringIMAP t
              showQuery (SINCEs t)      = "SINCE " ++ dateToStringIMAP t
              showQuery (SMALLERs siz)  = "SMALLER {" ++ show siz ++ "}"
              showQuery (SUBJECTs s)    = "SUBJECT " ++ s
              showQuery (TEXTs s)       = "TEXT " ++ s
              showQuery (TOs addr)      = "TO " ++ addr
              showQuery (UIDs uids)     = concat $ intersperse "," $
                                          map show uids
              showFlag Seen        = "SEEN"
              showFlag Answered    = "ANSWERED"
              showFlag Flagged     = "FLAGGED"
              showFlag Deleted     = "DELETED"
              showFlag Draft       = "DRAFT"
              showFlag Recent      = "RECENT"
              showFlag (Keyword s) = "KEYWORD " ++ s

data FlagsQuery = ReplaceFlags [Flag]
                | PlusFlags [Flag]
                | MinusFlags [Flag]

----------------------------------------------------------------------
-- establish connection

connectIMAPPort :: MonadIO m => String -> PortNumber -> m (IMAPConnection m)
connectIMAPPort hostname port = do
    h <- liftIO $ connectTo hostname (PortNumber port)
    connectStream (handleToStream h)

connectIMAP :: MonadIO m => String -> m (IMAPConnection m)
connectIMAP hostname = connectIMAPPort hostname 143

connectStream :: MonadIO m => BSStream m -> m (IMAPConnection m)
connectStream s =
    do msg <- bsGetLine s
       unless (and $ BS.zipWith (==) msg (BS.pack "* OK")) $
              fail "cannot connect to the server"
       newConnection s

----------------------------------------------------------------------
-- normal send commands
sendCommand' :: (MonadIO m,HasIMAPConnection m) => String -> m (ByteString, Int)
sendCommand' cmdstr = do
  (_, num) <- withNextCommandNum $ \num -> do
              s <- getBSStream
              bsPutCrLf s (BS.pack $ show6 num ++ " " ++ cmdstr)
  resp <- withBSStream getResponse
  return (resp, num)

show6 :: (Ord a, Num a, Show a) => a -> String
show6 n | n > 100000 = show n
        | n > 10000  = '0' : show n
        | n > 1000   = "00" ++ show n
        | n > 100    = "000" ++ show n
        | n > 10     = "0000" ++ show n
        | otherwise  = "00000" ++ show n

sendCommand :: (MonadIO m,HasIMAPConnection m)
            => String
            -> (RespDerivs -> Result RespDerivs (ServerResponse, MboxUpdate, v))
            -> m v
sendCommand cmdstr pFunc =
    do (buf, num) <- sendCommand' cmdstr
       let (resp, mboxUp, value) = eval pFunc (show6 num) buf
       case resp of
         OK _ _        -> do mboxUpdate mboxUp
                             return value
         NO _ msg      -> fail ("NO: " ++ msg)
         BAD _ msg     -> fail ("BAD: " ++ msg)
         PREAUTH _ msg -> fail ("preauth: " ++ msg)

getResponse :: Monad m => BSStream m -> m ByteString
getResponse s = unlinesCRLF <$> getLs
    where unlinesCRLF = BS.concat . concatMap (:[crlfStr])
          getLs =
              do l <- strip <$> bsGetLine s
                 case () of
                   _ | isLiteral l ->  do l' <- getLiteral l (getLitLen l)
                                          ls <- getLs
                                          return (l' : ls)
                     | isTagged l -> (l:) <$> getLs
                     | otherwise -> return [l]
          getLiteral l len =
              do lit <- bsGet s len
                 l2 <- strip <$> bsGetLine s
                 let l' = BS.concat [l, crlfStr, lit, l2]
                 if isLiteral l2
                   then getLiteral l' (getLitLen l2)
                   else return l'
          crlfStr = BS.pack "\r\n"
          isLiteral l = BS.last l == '}' &&
                        BS.last (fst (BS.spanEnd isDigit (BS.init l))) == '{'
          getLitLen = read . BS.unpack . snd . BS.spanEnd isDigit . BS.init
          isTagged l = BS.head l == '*' && BS.head (BS.tail l) == ' '

mboxUpdate :: (MonadIO m,HasIMAPConnection m) => MboxUpdate -> m ()
mboxUpdate (MboxUpdate exists' recent') = do
  when (isJust exists') $
       modifyMailboxInfo $ \mbox -> mbox { _exists = fromJust exists' }

  when (isJust recent') $
       modifyMailboxInfo $ \mbox -> mbox { _recent = fromJust recent' }

----------------------------------------------------------------------
-- IMAP commands
--

idle :: (MonadIO m,HasIMAPConnection m) => Int -> m ()
idle timeout =
    do
        (buf',num) <- sendCommand' "IDLE"
        buf <-
            if BS.take 2 buf' == BS.pack "+ "
                then do
                    s <- getBSStream
                    _ <- bsWaitForInput s timeout
                    bsPutCrLf s $ BS.pack "DONE"
                    getResponse s
                else
                    return buf'
        let (resp, mboxUp, value) = eval pNone (show6 num) buf
        case resp of
         OK _ _        -> do mboxUpdate mboxUp
                             return value
         NO _ msg      -> fail ("NO: " ++ msg)
         BAD _ msg     -> fail ("BAD: " ++ msg)
         PREAUTH _ msg -> fail ("preauth: " ++ msg)

noop :: (MonadIO m,HasIMAPConnection m) => m ()
noop = sendCommand "NOOP" pNone

capability :: (MonadIO m,HasIMAPConnection m) => m [String]
capability = sendCommand "CAPABILITY" pCapability

logout :: (MonadIO m,HasIMAPConnection m) => m ()
logout = do s <- getBSStream
            bsPutCrLf s $ BS.pack "a0001 LOGOUT"
            bsClose s

login :: (MonadIO m,HasIMAPConnection m)
      => A.UserName
      -> A.Password
      -> m ()
login username password = sendCommand
    ( "LOGIN "
    ++ escapeLogin username
    ++ " "
    ++ escapeLogin password
    ) pNone

authenticate :: (MonadIO m,HasIMAPConnection m)
             => A.AuthType
             -> A.UserName
             -> A.Password
             -> m ()
authenticate A.LOGIN username password =
    do (_, num) <- sendCommand' "AUTHENTICATE LOGIN"
       s <- getBSStream
       bsPutCrLf s $ BS.pack userB64
       bsGetLine s
       bsPutCrLf s $ BS.pack passB64
       buf <- getResponse s
       let (resp, mboxUp, value) = eval pNone (show6 num) buf
       case resp of
         OK _ _        -> do mboxUpdate mboxUp
                             return value
         NO _ msg      -> fail ("NO: " ++ msg)
         BAD _ msg     -> fail ("BAD: " ++ msg)
         PREAUTH _ msg -> fail ("preauth: " ++ msg)
    where (userB64, passB64) = A.login username password
authenticate at username password =
    do (c, num) <- sendCommand' $ "AUTHENTICATE " ++ show at
       let challenge =
               if BS.take 2 c == BS.pack "+ "
               then A.b64Decode $ BS.unpack $ head $
                    dropWhile (isSpace . BS.last) $ BS.inits $ BS.drop 2 c
               else ""
       s <- getBSStream
       bsPutCrLf s $ BS.pack $ A.auth at challenge username password
       buf <- getResponse s
       let (resp, mboxUp, value) = eval pNone (show6 num) buf
       case resp of
         OK _ _        -> do mboxUpdate mboxUp
                             return value
         NO _ msg      -> fail ("NO: " ++ msg)
         BAD _ msg     -> fail ("BAD: " ++ msg)
         PREAUTH _ msg -> fail ("preauth: " ++ msg)

_select :: (MonadIO m,HasIMAPConnection m) => String -> String -> m ()
_select cmd mboxName =
    do mbox' <- sendCommand (cmd ++ quoted mboxName) pSelect
       setMailboxInfo $ mbox' { _mailbox = mboxName }
    where
       quoted s = "\"" ++ s ++ "\""

select :: (MonadIO m,HasIMAPConnection m) => MailboxName -> m ()
select = _select "SELECT "

examine :: (MonadIO m,HasIMAPConnection m) => MailboxName -> m ()
examine = _select "EXAMINE "

create :: (MonadIO m,HasIMAPConnection m) => MailboxName -> m ()
create mboxname = sendCommand ("CREATE " ++ mboxname) pNone

delete :: (MonadIO m,HasIMAPConnection m) => MailboxName -> m ()
delete mboxname = sendCommand ("DELETE " ++ mboxname) pNone

rename :: (MonadIO m,HasIMAPConnection m) => MailboxName -> MailboxName -> m ()
rename mboxorg mboxnew =
    sendCommand ("RENAME " ++ mboxorg ++ " " ++ mboxnew) pNone

subscribe :: (MonadIO m,HasIMAPConnection m) => MailboxName -> m ()
subscribe mboxname = sendCommand ("SUBSCRIBE " ++ mboxname) pNone

unsubscribe :: (MonadIO m,HasIMAPConnection m) => MailboxName -> m ()
unsubscribe mboxname = sendCommand ("UNSUBSCRIBE " ++ mboxname) pNone

list :: (MonadIO m,HasIMAPConnection m) => m [([Attribute], MailboxName)]
list = (map (\(a, _, m) -> (a, m))) <$> listFull "\"\"" "*"

lsub :: (MonadIO m,HasIMAPConnection m) => m [([Attribute], MailboxName)]
lsub = (map (\(a, _, m) -> (a, m))) <$> lsubFull "\"\"" "*"

listFull :: (MonadIO m,HasIMAPConnection m) => String -> String
         -> m [([Attribute], String, MailboxName)]
listFull ref pat = sendCommand (unwords ["LIST", ref, pat]) pList

lsubFull :: (MonadIO m,HasIMAPConnection m) => String -> String
         -> m [([Attribute], String, MailboxName)]
lsubFull ref pat = sendCommand (unwords ["LSUB", ref, pat]) pLsub

status :: (MonadIO m,HasIMAPConnection m) => MailboxName -> [MailboxStatus]
       -> m [(MailboxStatus, Integer)]
status mbox stats =
    let cmd = "STATUS " ++ mbox ++ " (" ++ (unwords $ map show stats) ++ ")"
    in sendCommand cmd pStatus

append :: (MonadIO m,HasIMAPConnection m) => MailboxName -> ByteString -> m ()
append mbox mailData = appendFull mbox mailData [] Nothing

appendFull :: (MonadIO m,HasIMAPConnection m) => MailboxName -> ByteString
           -> [Flag] -> Maybe CalendarTime -> m ()
appendFull mbox mailData flags' time =
    do (buf, num) <- sendCommand'
                (unwords ["APPEND", mbox
                         , fstr, tstr,  "{" ++ show len ++ "}"])
       unless (BS.null buf || (BS.head buf /= '+')) $
              fail "illegal server response"
       s <- getBSStream
       mapM_ (bsPutCrLf s) mailLines
       buf2 <- getResponse s
       let (resp, mboxUp, ()) = eval pNone (show6 num) buf2
       case resp of
         OK _ _ -> mboxUpdate mboxUp
         NO _ msg -> fail ("NO: "++msg)
         BAD _ msg -> fail ("BAD: "++msg)
         PREAUTH _ msg -> fail ("PREAUTH: "++msg)
    where mailLines = BS.lines mailData
          len       = sum $ map ((2+) . BS.length) mailLines
          tstr      = maybe "" show time
          fstr      = unwords $ map show flags'

check :: (MonadIO m,HasIMAPConnection m) => m ()
check = sendCommand "CHECK" pNone

close :: (MonadIO m,HasIMAPConnection m) => m ()
close =
    do sendCommand "CLOSE" pNone
       setMailboxInfo emptyMboxInfo

expunge :: (MonadIO m,HasIMAPConnection m) => m [Integer]
expunge = sendCommand "EXPUNGE" pExpunge

search :: (MonadIO m,HasIMAPConnection m)
       => [SearchQuery]
       -> m [UID]
search queries = searchCharset "" queries

searchCharset :: (MonadIO m,HasIMAPConnection m) => Charset -> [SearchQuery]
              -> m [UID]
searchCharset charset queries =
    sendCommand ("UID SEARCH "
                    ++ (if not . null $ charset
                           then charset ++ " "
                           else "")
                    ++ unwords (map show queries)) pSearch

fetch :: (MonadIO m,HasIMAPConnection m) => UID -> m ByteString
fetch uid =
    do lst <- fetchByString uid "BODY[]"
       return $ maybe BS.empty BS.pack $ lookup' "BODY[]" lst

fetchHeader :: (MonadIO m,HasIMAPConnection m)
            => UID
            -> m ByteString
fetchHeader uid =
    do lst <- fetchByString uid "BODY[HEADER]"
       return $ maybe BS.empty BS.pack $ lookup' "BODY[HEADER]" lst

fetchSize :: (MonadIO m,HasIMAPConnection m) => UID -> m Int
fetchSize uid =
    do lst <- fetchByString uid "RFC822.SIZE"
       return $ maybe 0 read $ lookup' "RFC822.SIZE" lst

fetchHeaderFields :: (MonadIO m,HasIMAPConnection m)
                  => UID
                  -> [String]
                  -> m ByteString
fetchHeaderFields uid hs =
    do lst <- fetchByString uid ("BODY[HEADER.FIELDS "++unwords hs++"]")
       return $ maybe BS.empty BS.pack $
              lookup' ("BODY[HEADER.FIELDS "++unwords hs++"]") lst

fetchHeaderFieldsNot :: (MonadIO m,HasIMAPConnection m)
                     => UID
                     -> [String]
                     -> m ByteString
fetchHeaderFieldsNot uid hs =
    do let fetchCmd = "BODY[HEADER.FIELDS.NOT "++unwords hs++"]"
       lst <- fetchByString uid fetchCmd
       return $ maybe BS.empty BS.pack $ lookup' fetchCmd lst

fetchFlags :: (MonadIO m,HasIMAPConnection m) => UID -> m [Flag]
fetchFlags uid =
    do lst <- fetchByString uid "FLAGS"
       return $ getFlags $ lookup' "FLAGS" lst
    where getFlags Nothing  = []
          getFlags (Just s) = eval' dvFlags "" s

fetchR :: (MonadIO m,HasIMAPConnection m) => (UID, UID)
       -> m [(UID, ByteString)]
fetchR r =
    do lst <- fetchByStringR r "BODY[]"
       return $ map (\(uid, vs) -> (uid, maybe BS.empty BS.pack $
                                       lookup' "BODY[]" vs)) lst
fetchByString :: (MonadIO m,HasIMAPConnection m) => UID -> String
              -> m [(String, String)]
fetchByString uid command =
    do lst <- fetchCommand ("UID FETCH "++show uid++" "++command) id
       return $ snd $ head lst

fetchByStringR :: (MonadIO m,HasIMAPConnection m) => (UID, UID) -> String
               -> m [(UID, [(String, String)])]
fetchByStringR (s, e) command =
    fetchCommand ("UID FETCH "++show s++":"++show e++" "++command) proc
    where proc (n, ps) =
              (maybe (toEnum (fromIntegral n)) read (lookup' "UID" ps), ps)

fetchCommand :: (MonadIO m,HasIMAPConnection m) => String
             -> ((Integer, [(String, String)]) -> b) -> m [b]
fetchCommand command proc =
    (map proc) <$> sendCommand command pFetch

storeFull :: (MonadIO m,HasIMAPConnection m) => String -> FlagsQuery -> Bool
          -> m [(UID, [Flag])]
storeFull uidstr query isSilent =
    fetchCommand ("UID STORE " ++ uidstr ++ " " ++ flgs query) procStore
    where fstrs fs = "(" ++ (concat $ intersperse " " $ map show fs) ++ ")"
          toFStr s fstrs' =
              s ++ (if isSilent then ".SILENT" else "") ++ " " ++ fstrs'
          flgs (ReplaceFlags fs) = toFStr "FLAGS" $ fstrs fs
          flgs (PlusFlags fs)    = toFStr "+FLAGS" $ fstrs fs
          flgs (MinusFlags fs)   = toFStr "-FLAGS" $ fstrs fs
          procStore (n, ps) = (maybe (toEnum (fromIntegral n)) read
                                         (lookup' "UID" ps)
                              ,maybe [] (eval' dvFlags "") (lookup' "FLAG" ps))


store :: (MonadIO m,HasIMAPConnection m) => UID -> FlagsQuery -> m ()
store i q = storeFull (show i) q True >> return ()

copyFull :: (MonadIO m,HasIMAPConnection m) => String -> String -> m ()
copyFull uidStr mbox =
    sendCommand ("UID COPY " ++ uidStr ++ " " ++ mbox) pNone

copy :: (MonadIO m,HasIMAPConnection m) => UID -> MailboxName -> m ()
copy uid mbox     = copyFull (show uid) mbox

----------------------------------------------------------------------
-- auxialiary functions

dateToStringIMAP :: CalendarTime -> String
dateToStringIMAP date = concat $ intersperse "-" [show2 $ ctDay date
                                                 , showMonth $ ctMonth date
                                                 , show $ ctYear date]
    where show2 n | n < 10    = '0' : show n
                  | otherwise = show n
          showMonth January   = "Jan"
          showMonth February  = "Feb"
          showMonth March     = "Mar"
          showMonth April     = "Apr"
          showMonth May       = "May"
          showMonth June      = "Jun"
          showMonth July      = "Jul"
          showMonth August    = "Aug"
          showMonth September = "Sep"
          showMonth October   = "Oct"
          showMonth November  = "Nov"
          showMonth December  = "Dec"

strip :: ByteString -> ByteString
strip = fst . BS.spanEnd isSpace . BS.dropWhile isSpace

crlf :: BS.ByteString
crlf = BS.pack "\r\n"

bsPutCrLf :: Monad m => BSStream m -> ByteString -> m ()
bsPutCrLf h s = bsPut h s >> bsPut h crlf >> bsFlush h

lookup' :: String -> [(String, b)] -> Maybe b
lookup' _ [] = Nothing
lookup' q ((k,v):xs) | q == lastWord k  = return v
                     | otherwise        = lookup' q xs
    where
        lastWord = last . words

-- TODO: This is just a first trial solution for this stack overflow question:
--       http://stackoverflow.com/questions/26183675/error-when-fetching-subject-from-email-using-haskellnets-imap
--       It must be reviewed. References: rfc3501#6.2.3, rfc2683#3.4.2.
--       This function was tested against the password: `~1!2@3#4$5%6^7&8*9(0)-_=+[{]}\|;:'",<.>/? (with spaces in the laterals).
escapeLogin :: String -> String
escapeLogin x = "\"" ++ replaceSpecialChars x ++ "\""
    where
        replaceSpecialChars ""     = ""
        replaceSpecialChars (c:cs) = escapeChar c ++ replaceSpecialChars cs
        escapeChar '"' = "\\\""
        escapeChar '\\' = "\\\\"
        escapeChar '{' = "\\{"
        escapeChar '}' = "\\}"
        escapeChar s   = [s]
