module Push where

import           Options.Applicative
import           System.Directory    (doesDirectoryExist)
import           System.IO           (hGetContents)
import           System.Process      (StdStream (CreatePipe), createProcess,
                                      cwd, proc, readProcess, std_err, std_out,
                                      waitForProcess)

import           Datatypes
import           Remote

pushOptions :: Parser Cmd
pushOptions = Push
  <$> (PushOptions
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
      <> metavar "REPOPREFIX"
      <> help "pull only repositories starting with PREFIX" )
    <*> strOption
        ( long "branch"
      <> short 'b'
      <> value "master"
      <> metavar "BRANCH"
      <> help "pull BRANCH" )
  )

push :: PushOptions -> IO ()
push opts = do
    repoList <- getRemoteRepoList
                    (pushUser   opts)
                    (pushHost   opts)
                    (pushPort   opts)
                    (pushPrefix opts)
    mapM_ (pushRepo opts) repoList

pushRepo :: PushOptions -> FilePath -> IO ()
pushRepo opts repo = do
    let remoteRepo = "ssh://" ++ pushUser opts ++ "@" ++ pushHost opts
                 ++ ":" ++ pushPort opts ++ "/" ++ repo
    let remoteName = "tmpRemote"
    putStrLn $ "Pushing to \x1b[34m" ++ remoteRepo ++ "\x1b[0m"
    _ <- readProcess "git" ["remote", "add", remoteName, remoteRepo] ""
    pushInfo <- readProcess "git" ["push", remoteName, pushBranch opts] ""
    putStrLn pushInfo
    _ <- readProcess "git" ["remote", "remove", remoteName] ""
    return ()

