{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Serf where
import Control.Applicative
import Control.Monad.Operational
import Control.Monad.Reader
import Data.Attoparsec.Text hiding (Done)
import Data.Char
import Data.Conduit.Attoparsec
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.Internal (ConduitM(..), Pipe(Done))
import qualified Data.Conduit.List as L
import qualified Data.Conduit.Text as T
import Data.List (lookup)
import Data.Text (Text)
import System.Environment
import System.Exit
import System.IO (Handle, hClose)
import System.Process

{-
data Configuration = Configuration
  { _confNodeName
  , _confRole
  , _confBindAddr
  , _confEncryptKey
  , _confLogLevel
  , _confProtocol
  , _confRpcAddr
  , _confEventHandlers :: [EventHandler]
  , _confStartJoin :: [String]
  }

-}

data StartAgentOptions = StartAgentOptions
data MonitorOptions = MonitorOptions
  { _moLogLevel :: Maybe LogLevel
  }

data LogLevel = Trace | Debug | Info | Warn | Error

type ResIO = ResourceT IO

logStr :: LogLevel -> String
logStr l = case l of
  Trace -> "trace"
  Debug -> "debug"
  Info -> "info"
  Warn -> "warn"
  Error -> "err"

  --ReadAgentConfig :: FilePath -> Serf (Maybe AgentConfig)
  --WriteAgentConfig :: FilePath -> AgentConfig -> Serf ()
  --GetEnv :: Serf SerfEnv

sendEvent :: String -> Maybe String -> SerfM Bool
sendEvent n p = sendEvent' (SendOptions Nothing) n p

-- | Dispatch a custom user event into a Serf cluster.
--
-- Nodes in the cluster listen for these custom events and react to them.
--class SendEvent a where
--  sendEvent :: String -> Maybe String -> a
sendEvent' :: SendOptions -> String -> Maybe String -> SerfM Bool
sendEvent' o n p = singleton $ SendEvent o n p

forceLeave :: String -> SerfM Bool
forceLeave = singleton . ForceLeave

joinNodes :: String -> [String] -> SerfM Bool
joinNodes n ns = joinNodes' (JoinOptions Nothing) n ns

joinNodes' :: JoinOptions -> String -> [String] -> SerfM Bool
joinNodes' o n ns = singleton $ JoinNodes o n ns

members :: SerfM (Maybe [MemberStatus])
members = singleton Members

data Serf a where
  StartAgent :: StartAgentOptions -> Serf Bool
  SendEvent :: SendOptions -> String -> Maybe String -> Serf Bool
  ForceLeave :: String -> Serf Bool
  JoinNodes :: JoinOptions -> String -> [String] -> Serf Bool
  Members :: Serf (Maybe [MemberStatus])
  --DetailedMembers :: Serf [DetailedMemberStatus]
  Monitor :: MonitorOptions -> Serf (Maybe (Source ResIO Text))

type SerfM = Program Serf

class MonadSerf m where
  evalSerf :: Serf a -> m a

--instance MonadSerf

serf :: SerfM a -> IO a
serf m = serfWithOpts [] m

serfAt :: String -> SerfM a -> IO a
serfAt str = serfWithOpts ["-rpc-addr=" ++ str]

serfWithOpts :: [String] -> SerfM a -> IO a
serfWithOpts globals m = case view m of
  Return a -> return a
  --(StartAgent opts) :>>= k -> do
  --  b <- ___
  --  serf $ k b
  (SendEvent opts name mPayload) :>>= k -> do
    b <- sendEventCore globals opts name mPayload
    serf $ k b
  (ForceLeave node) :>>= k -> do
    b <- forceLeaveCore globals node
    serf $ k b
  (JoinNodes opts aNode rest) :>>= k -> do
    b <- joinNodesCore globals opts aNode rest
    serf $ k b
  Members :>>= k -> do
    b <- membersCore globals
    serf $ k b
  --DetailedMembers :>>= k -> do
  --  b <- ___
  --  serf $ k b
  --(Monitor opts) :>>= k -> do
    --b <- ___
    --serf $ k b

data MemberInfo = MemberInfo
  { _miName :: String
  , _miAddress :: String
  , _miRole :: String
  }

data LastKnownStatus = Alive | Failed
  deriving (Read, Show)

data MemberStatus = MemberStatus
  { _msName :: Text
  , _msAddress :: Text
  , _msStatus :: LastKnownStatus
  } deriving (Read, Show)

{-
memberChanges :: Producer IO ByteString -> Producer IO MemberInfo



data EventHandler = EventHandler HandlerFilter String
data HandledEvent = HandleEvent Event | AllUserEvents
data HandlerFilter = Always | When [HandledEvent]

getConfig
setConfig
startAgent
stopAgent
-}

data SendOptions = SendOptions
  { _sendCoalesce :: Maybe Bool
  }

bool :: Bool -> String
bool False = "false"
bool True = "true"

serfCmd :: String -> [String] -> [String] -> CreateProcess
serfCmd cmd rest opts = proc "serf" (cmd : (opts ++ rest))

toList = maybe [] (:[])



-- | Allows specifiying a custom set of send options when sending an event.
type WithSendOptions = SendOptions -> IO Bool

-- | Allows modifying the default send options when sending an event.
type ModifyDefaultSendOptions = (SendOptions -> SendOptions) -> IO Bool

-- | Send an event with a complete custom set of send options.
--instance SendEvent WithSendOptions where
sendEventCore :: [String] -> SendOptions -> String -> Maybe String -> IO Bool
sendEventCore globalOpts options name mPayload = do
  (_, _, _, process) <- createProcess processSettings
  exitCode <- waitForProcess process
  return $! exitCode == ExitSuccess
  where 
    payload = toList mPayload
    processSettings = serfCmd "event" (optionArgs ++ [name] ++ payload) globalOpts
    optionArgs = toList sendCoalesce
    sendCoalesce = fmap (\o -> "-coalesce=" ++ bool o) $ _sendCoalesce options

data ForceLeaveOptions = ForceLeaveOptions
  { _flRpcAddr :: Maybe String
  } deriving (Read, Show)

forceLeaveCore :: [String] -> String -> IO Bool
forceLeaveCore globalOpts node = do
  (_, _, _, process) <- createProcess $ processSettings
  exitCode <- waitForProcess process
  return $! exitCode == ExitSuccess
  where
      processSettings = serfCmd "force-leave" [node] globalOpts

data JoinOptions = JoinOptions
  { _jsReplay :: Maybe Bool
  }

joinNodesCore :: [String] -> JoinOptions -> String -> [String] -> IO Bool
joinNodesCore globalOpts opts aNode rest = do
  (_, _, _, process) <- createProcess $ processSettings
  exitCode <- waitForProcess process
  return $! exitCode == ExitSuccess
  where
    processSettings = serfCmd "join" (aNode : rest) globalOpts

memberParser :: Parser MemberStatus
memberParser = do
  n <- takeTill isSpace
  skipSpace
  a <- takeTill isSpace
  skipSpace
  s <- takeTill isSpace
  skipSpace
  return $ MemberStatus n a $ status s
  where
    status s = if s == "alive"
      then Alive
      else Failed

readMemberList :: Handle -> IO [MemberStatus]
readMemberList h = runResourceT (bracketP (return h) hClose source $$ L.consume)
  where
    source h = mapOutput snd (sourceHandle h $= T.decode T.utf8 $= conduitParser memberParser)

membersCore :: [String] -> IO (Maybe [MemberStatus])
membersCore globalOpts = do
  mh <- membersOutput globalOpts
  case mh of
    Nothing -> return Nothing
    Just h -> do
      l <- readMemberList h
      return $! Just l

membersOutput :: [String] -> IO (Maybe Handle)
membersOutput globalOpts = do
  (_, (Just h), _, process) <- createProcess $ processSettings { std_out = CreatePipe }
  exitCode <- waitForProcess process
  if exitCode == ExitSuccess
    then return $ Just h
    else return Nothing
  where
    processSettings = serfCmd "members" [] globalOpts

monitorCore :: [String] -> MonitorOptions -> IO (Maybe (Source ResIO Text))
monitorCore globalOpts opts = do
  (_, (Just h), _, process) <- createProcess $ processSettings { std_out = CreatePipe }
  exitCode <- waitForProcess process
  if exitCode == ExitSuccess
    then return $ Just undefined
    else return Nothing
  where
    processSettings = serfCmd "monitor" (toList $ fmap logStr $ _moLogLevel opts) globalOpts
    src = do
      (_, (Just h), _, process) <- createProcess $ processSettings { std_out = CreatePipe }
      mExit <- getProcessExitCode process
      if mExit == Nothing
        then undefined
        else undefined

linesUntilNewline :: Conduit Text ResIO Text
linesUntilNewline = do
  mLine <- await
  liftIO $ print mLine
  case mLine of
    Nothing -> ConduitM $ Done ()
    Just l -> if l == ""
      then ConduitM $ Done ()
      else yield l >> linesUntilNewline