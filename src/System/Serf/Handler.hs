-- | This module provides basic support for implementing Serf event handler programs
-- using Haskell. Serf calls event handlers by executing the specified handler program
-- with information relevant to the handler set in environment variables.
module System.Serf.Handler (
  Event(..),
  SerfEnv(..),
  getSerfEnv
) where
import Control.Applicative
import System.Environment

-- | A union of all possible event types that Serf supports.
data Event
  = MemberJoin -- ^ An event indicating that members have joined the cluster.
  | MemberLeave -- ^ An event indicating that members have left the cluster.
  | MemberFailed -- ^ An event indicating that members have failed out of the cluster.
  | User String -- ^ A custom event triggered by an agent in the cluster.
  deriving (Read, Show)

-- | All data set in the environment for the event handler. This is the primary way that Serf
-- communicates relevant information to the executing handler.
data SerfEnv = SerfEnv
  { event :: Event -- ^ The event that caused the handling program to be executed.
  , selfName :: String -- ^ The name of the node that is executing the event handler.
  , selfRole :: String -- ^ The role of the node that is executing the event handler.
  } deriving (Read, Show)

-- | Retrieve all environment info set by Serf, returning Nothing if any Serf environment data
-- is missing.
getSerfEnv :: IO (Maybe SerfEnv)
getSerfEnv = do
  env <- getEnvironment
  return $! SerfEnv <$>
    serfEvent env <*>
    lookup "SERF_SELF_NAME" env <*>
    lookup "SERF_SELF_ROLE" env
  where
    serfEvent env = do
      case lookup "SERF_EVENT" env of
        Nothing -> Nothing
        Just e -> case e of
          "member-join" -> Just MemberJoin
          "member-leave" -> Just MemberLeave
          "member-failed" -> Just MemberFailed
          "user" -> User <$> lookup "SERF_USER_EVENT" env
