module System.Serf.Handler where

data Event
  = MemberJoin
  | MemberLeave
  | MemberFailed
  | User String
  deriving (Read, Show)

data SerfEnv = SerfEnv
  { _seEvent :: Event
  , _seSelfName :: String
  , _seSelfRole :: String
  } deriving (Read, Show)

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
