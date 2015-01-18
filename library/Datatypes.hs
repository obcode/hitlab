module Datatypes where

import           Data.Text

data Conf = Conf
    { conffile :: FilePath
    , cmd      :: Cmd
    }

data RichConf = RichConf
    { conf         :: Conf
    , maybeLogfile :: Maybe Text
    , maybeRepoPrg :: Maybe Text
    }

data DefaultOptions

data Cmd = Create CreateOptions
         | Pull PullOptions
         | Push PushOptions

data CreateOptions = CreateOptions
    { createPrefix   :: String
    , createDesc     :: String
    , createInfile   :: FilePath
    , createOutfile  :: FilePath
    , createReadOnly :: Bool
    }

data PullOptions = PullOptions
    { pullUser     :: String
    , pullHost     :: String
    , pullPort     :: String
    , pullPrefix   :: String -- Pull only repos starting with the prefix
    , pullOpenWith :: String
    }

data PushOptions = PushOptions
    { pushUser   :: String
    , pushHost   :: String
    , pushPort   :: String
    , pushPrefix :: String -- Push only to repos starting with the prefix
    , pushBranch :: String -- Push branch (default: master)
    }

data Group = Group
  { groupname    :: String
  , groupmembers :: [String]
  }
