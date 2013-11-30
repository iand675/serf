{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
-- | \"Serf is a service discovery and orchestration tool that is decentralized, highly available, and fault tolerant. Serf runs on every major platform: Linux, Mac OS X, and Windows. It is extremely lightweight: it uses 5 to 10 MB of resident memory and primarily communicates using infrequent UDP messages.\"
--
-- <www.serfdom.io/intro> 
--
-- This module provides facilities for interacting with a serf agent running on a machine. This module aims to expose all functionality
-- provided by the serf command-line tool in a programmatic way.
module System.Serf (
  SerfM,
  Serf(..),
  MonadSerf(..),
  -- StartAgentOptions(..),
  serf,
  serfAt,
  serfWithOpts,
  sendEvent,
  sendEvent',
  SendOptions(..),
  forceLeave,
  joinNodes,
  joinNodes',
  JoinOptions(..),
  members,
  MemberStatus(..),
  LastKnownStatus(..),
  LogLevel(..),
  MonitorOptions(..)
) where
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

-- | Initialization options for starting the serf agent.
data StartAgentOptions = StartAgentOptions
  { agentNodeName :: Maybe String
  -- ^ An optional node name for the agent.
  --
  -- Defaults to the hostname of the machine.
  , agentRole :: Maybe String
  -- ^ An optional role for the node. This can be used to differenatiate nodes in the cluster that have different roles.
  --
  -- Empty by default.
  , agentBindAddr :: Maybe String
  -- ^ The address that the agent will bind to for communication with other Serf nodes.
  --
  -- Defaults to 0.0.0.0:7946
  , agentEncryptKey :: Maybe String
  -- ^ An optional encryption key created by running the serf keygen command. All nodes in the cluster must use the same
  -- encryption key in order to communicate.
  , agentLogLevel :: Maybe LogLevel
  -- ^ The level of logging to show after the agent has started.
  , agentProtocol :: Maybe Int
  -- ^ The serf protocol version to use.
  , agentRpcAddr :: Maybe String
  -- ^ The address that serf will bind to for the agent's internal RPC server.
  --
  -- Defaults to 127.0.0.1:7373.
  , agentEventHandlers :: [String]
  -- ^ A list of event handlers following the syntax specified at <http://www.serfdom.io/docs/agent/event-handlers.html>.
  --
  -- Handlers will be separated by commas automatically.
  , agentStartJoin :: [String]
  -- ^ A list of nodes to join immediately upon startup.
  --
  -- If joining any of the specified nodes fails, the agent will fail to start
  }

-- | Options for monitoring serf agent events. It is recommended that the log level is cranked up
-- to either "Warn" or "Error", as the default currently seems to be "Debug", and is not generally
-- useful in production environments.
data MonitorOptions = MonitorOptions
  { monitorLogLevel :: Maybe LogLevel
  }

-- | The minimum log level to log with the "monitor" command.
data LogLevel
  = Trace
  | Debug
  | Info
  | Warn
  | Error

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

-- | Dispatch a custom user event into a Serf cluster.
--
-- Nodes in the cluster listen for these custom events and react to them.
sendEvent :: String -- ^ The name of the custom event.
  -> Maybe String -- ^ An optional payload to be sent with the event.
  -> SerfM Bool -- ^ Whether the event was successfully sent.
sendEvent n p = sendEvent' (SendOptions Nothing) n p

-- | Dispatch a custom user event into a Serf cluster with additional flags set.
sendEvent' :: SendOptions -> String -> Maybe String -> SerfM Bool
sendEvent' o n p = singleton $ SendEvent o n p

-- | Force a specific node to leave a cluster. Note that the node will
-- rejoin unless the serf agent for that node has exited.
forceLeave :: String -> SerfM Bool
forceLeave = singleton . ForceLeave

-- | Join the node to a cluster using the specified address(es).
--
-- At least one node address must be specified.
joinNodes :: String -- ^ The first node to join.
  -> [String] -- ^ Additional nodes to join.
  -> SerfM Bool -- ^ Whether joining all nodes was successful.
joinNodes n ns = joinNodes' (JoinOptions Nothing) n ns

-- | Join the node to a cluster with non-standard options.
joinNodes' :: JoinOptions -> String -> [String] -> SerfM Bool
joinNodes' o n ns = singleton $ JoinNodes o n ns

-- | List known members in the cluster
members :: SerfM [MemberStatus]
members = singleton Members

-- | Commands supported by the serf executable (serf protocol v1).
data Serf a where
  -- StartAgent :: StartAgentOptions -> Serf Bool
  SendEvent :: SendOptions -> String -> Maybe String -> Serf Bool
  ForceLeave :: String -> Serf Bool
  JoinNodes :: JoinOptions -> String -> [String] -> Serf Bool
  Members :: Serf [MemberStatus]
  -- DetailedMembers :: Serf [DetailedMemberStatus]
  -- Monitor :: MonitorOptions -> Serf (Maybe (Source (ResourceT IO) Text))

-- | An alias for the operational monad created with the "Serf" data type.
type SerfM = Program Serf

-- | A convenience class for lifting serf action evaluation into monad transformer stacks.
class (Monad m) => MonadSerf m where
  -- | Evaluate the specified serf actions in given context
  evalSerf :: SerfM a -> m a

-- | Run serf actions locally on the default port.
serf :: SerfM a -> IO a
serf m = serfWithOpts [] m

-- | Run serf actions at a specified RPC address.
serfAt :: String -> SerfM a -> IO a
serfAt str = serfWithOpts ["-rpc-addr=" ++ str]

-- | Run serf actions with a list of arbitrary command line arguments.
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
  { memberName :: String
  , memberAddress :: String
  , memberRole :: String
  }

-- | The last known status of listed nodes.
data LastKnownStatus = Alive | Failed
  deriving (Read, Show)

data MemberStatus = MemberStatus
  { memberStatusName :: Text
  , memberStatusAddress :: Text
  , memberStatus :: LastKnownStatus
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
  { coalesceEvents :: Maybe Bool
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
    sendCoalesce = fmap (\o -> "-coalesce=" ++ bool o) $ coalesceEvents options

forceLeaveCore :: [String] -> String -> IO Bool
forceLeaveCore globalOpts node = do
  (_, _, _, process) <- createProcess $ processSettings
  exitCode <- waitForProcess process
  return $! exitCode == ExitSuccess
  where
      processSettings = serfCmd "force-leave" [node] globalOpts

-- | Options specific to joining a cluster
data JoinOptions = JoinOptions
  { _jsReplay :: Maybe Bool -- ^ Whether to replay all events that have occurred in the cluster.
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

membersCore :: [String] -> IO [MemberStatus]
membersCore globalOpts = do
  mh <- membersOutput globalOpts
  case mh of
    Nothing -> return []
    Just h -> do
      l <- readMemberList h
      return l

membersOutput :: [String] -> IO (Maybe Handle)
membersOutput globalOpts = do
  (_, (Just h), _, process) <- createProcess $ processSettings { std_out = CreatePipe }
  exitCode <- waitForProcess process
  if exitCode == ExitSuccess
    then return $ Just h
    else return Nothing
  where
    processSettings = serfCmd "members" [] globalOpts

monitorCore :: [String] -> MonitorOptions -> IO (Maybe (Source (ResourceT IO) Text))
monitorCore globalOpts opts = do
  (_, (Just h), _, process) <- createProcess $ processSettings { std_out = CreatePipe }
  exitCode <- waitForProcess process
  if exitCode == ExitSuccess
    then return $ Just undefined
    else return Nothing
  where
    processSettings = serfCmd "monitor" (toList $ fmap logStr $ monitorLogLevel opts) globalOpts
    src = do
      (_, (Just h), _, process) <- createProcess $ processSettings { std_out = CreatePipe }
      mExit <- getProcessExitCode process
      if mExit == Nothing
        then undefined
        else undefined

linesUntilNewline :: Conduit Text (ResourceT IO) Text
linesUntilNewline = do
  mLine <- await
  liftIO $ print mLine
  case mLine of
    Nothing -> ConduitM $ Done ()
    Just l -> if l == ""
      then ConduitM $ Done ()
      else yield l >> linesUntilNewline