module Datatypes where

data Conf = Conf
    { conffile :: FilePath
    , cmd      :: Cmd
    }
  deriving (Show)

data Cmd = Create CreateOptions
         | Pull PullOptions
         | Push PushOptions
  deriving (Show)

data CreateOptions = CreateOptions
    { createPrefix   :: String
    , createDesc     :: String
    , createInfile   :: FilePath
    , createOutfile  :: FilePath
    , createReadOnly :: Bool
    }
  deriving (Show)

data PullOptions = PullOptions
    { pullUser     :: String
    , pullHost     :: String
    , pullPort     :: String
    , pullPrefix   :: String -- Pull only repos starting with the prefix
    , pullBranch   :: String -- Pull branch (default: master)
    }
  deriving (Show)

data PushOptions = PushOptions
    { pushUser     :: String
    , pushHost     :: String
    , pushPort     :: String
    , pushPrefix   :: String -- Push only to repos starting with the prefix
    , pushBranch   :: String -- Push branch (default: master)
    }
  deriving (Show)

data Group = Group
  { groupname :: String
  , groupmembers :: [String]
  } deriving (Show)
