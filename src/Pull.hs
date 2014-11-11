{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Pull where

import           Control.Logging     (log)
import           Data.Data
import           Data.Function       (on)
import           Data.List           (groupBy, intercalate, isInfixOf, isPrefixOf,
                                      sort)
import           Data.Text           (pack)
import           Options.Applicative
import           Prelude             hiding (log)
import           System.Directory    (doesDirectoryExist)
import           System.IO           (hGetContents)
import           System.Process      (StdStream (CreatePipe), createProcess,
                                      cwd, proc, std_err, std_out,
                                      waitForProcess)

import           Datatypes
import           Remote
import           System.IO           (BufferMode (NoBuffering), hSetBuffering,
                                      stdout)

pullOptions = Pull
  <$> (PullOptions
    <$> strOption
        ( long "user"
      <> short 'u'
      <> value "git"
      <> metavar "USER"
      <> help "Git User on remote" )
    <*> strOption
        ( long "host"
      <> short 'h'
      <> value "ob.cs.hm.edu"
      <> metavar "HOST"
      <> help "Host running gitolite" )
    <*> strOption
        ( long "port"
      <> short 'p'
      <> value "8022"
      <> metavar "PORT"
      <> help "sshd listening on PORT" )
    <*> strOption
        ( long "repos"
      <> short 'r'
      <> value ""
      <> metavar "REPOPREFIX"
      <> help "pull only repositories starting with PREFIX" )
    <*> strOption
        ( long "branch"
      <> short 'b'
      <> value "master"
      <> metavar "BRANCH"
      <> help "pull BRANCH" )
  )

data PullResponse = Cloned String
                  | NoChanges String
                  | Changes String
                  | Fatal String
  deriving (Eq, Ord, Data, Show, Typeable)

pullResponseRepo :: PullResponse -> String
pullResponseRepo (Cloned    r) = r
pullResponseRepo (NoChanges r) = r
pullResponseRepo (Changes   r) = r
pullResponseRepo (Fatal     r) = r

pull :: PullOptions -> IO ()
pull opts = do
    repoList <- getRemoteRepoList
                    (pullUser   opts)
                    (pullHost   opts)
                    (pullPort   opts)
                    (pullPrefix opts)
    responses <- mapM (pullRepo opts) repoList
    putStrLn $ '\n' : stats responses
    log $ pack $ longstats responses
  where
    groups = groupBy (on (==) toConstr) . sort
    groupInfo = map getInfo . groups
    getInfo group@(Cloned _:_)    =
        "\x1b[34mCloned:     " ++ show ( length group ) ++ "\x1b[0m"
    getInfo group@(NoChanges _:_) =
        "\x1b[32mUp-to-date: " ++ show ( length group ) ++ "\x1b[0m"
    getInfo group@(Changes _:_)   =
        "\x1b[33mChanges:    " ++ show ( length group ) ++ "\x1b[0m"
    getInfo group@(Fatal _:_)   =
        "\x1b[31mFatal:      " ++ show ( length group ) ++ "\x1b[0m"
    stats responses = intercalate "\n" $ groupInfo responses
    getResponseString (Cloned    _:_) = "Cloned   "
    getResponseString (NoChanges _:_) = "NoChanges"
    getResponseString (Changes   _:_) = "Changes  "
    getResponseString (Fatal     _:_) = "Fatal    "
    getRepoInfo group = (getResponseString group, map pullResponseRepo group)
    format (resp, repos) = resp ++ ": " ++ intercalate "\n           " repos
    longstats = ('\n':) . intercalate "\n" . map (format . getRepoInfo) . groups

pullRepo :: PullOptions -> FilePath -> IO PullResponse
pullRepo opts repo = do
    hSetBuffering stdout NoBuffering
    putStr "."
    alreadyExist <- doesDirectoryExist repo
    if not alreadyExist
    then
      do log $ pack $ "Cloning " ++ repo
         (_, Just hout, Just herr, ph) <- createProcess
            (proc "git" ["clone"
            , "ssh://" ++ pullUser opts ++ "@" ++ pullHost opts
                ++ ":" ++ pullPort opts ++ "/" ++ repo , repo ])
                            { std_out = CreatePipe
                            , std_err = CreatePipe }
         cloneInfo <- hGetContents herr
         log $ pack $ cloneInfo
         waitForProcess ph
         return $ Cloned repo
    else
      do log $ pack $ "Pulling " ++ repo
         (_, Just hout, Just herr, ph) <- createProcess
            (proc "git" ["pull", "origin", pullBranch opts])
                            { cwd = Just repo
                            , std_out = CreatePipe
                            , std_err = CreatePipe }
         pullInfo <- hGetContents hout
         pullInfoErr <- hGetContents herr
         log $ pack $ pullInfoErr
         waitForProcess ph
         return $ (if "Already up-to-date" `isPrefixOf` pullInfo
                   then NoChanges
                   else if "fatal:" `isInfixOf` pullInfoErr
                       then Fatal
                       else Changes
                  ) repo

