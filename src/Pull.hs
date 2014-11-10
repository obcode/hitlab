module Pull where

import           Options.Applicative
import           System.Directory    (doesDirectoryExist)
import           System.IO           (hGetContents)
import           System.Process      (StdStream (CreatePipe), createProcess,
                                      cwd, proc, std_err, std_out,
                                      waitForProcess)

import           Datatypes
import           Remote

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

pull :: PullOptions -> IO ()
pull opts = do
    repoList <- getRemoteRepoList
                    (pullUser   opts)
                    (pullHost   opts)
                    (pullPort   opts)
                    (pullPrefix opts)
    mapM_ (pullRepo opts) repoList

pullRepo :: PullOptions -> FilePath -> IO ()
pullRepo opts repo = do
    alreadyExist <- doesDirectoryExist repo
    if not alreadyExist
    then
      do putStrLn $ "Cloning \x1b[34m" ++ repo ++ "\x1b[0m"
         (_, Just hout, _, ph) <- createProcess
            (proc "git" ["clone"
            , "ssh://" ++ pullUser opts ++ "@" ++ pullHost opts
                ++ ":" ++ pullPort opts ++ "/" ++ repo , repo ])
                            { std_out = CreatePipe
                            , std_err = CreatePipe }
         cloneInfo <- hGetContents hout
         putStrLn $ "\x1b[32m" ++ cloneInfo ++ "\x1b[0m"
         waitForProcess ph
         return ()
    else
      do putStrLn $ "Pulling \x1b[34m" ++ repo ++ "\x1b[0m"
         (_, Just hout, _, ph) <- createProcess
            (proc "git" ["pull", "origin", pullBranch opts])
                            { cwd = Just repo
                            , std_out = CreatePipe
                            , std_err = CreatePipe }
         pullInfo <- hGetContents hout
         putStrLn $ "\x1b[32m" ++ pullInfo ++ "\x1b[0m"
         waitForProcess ph
         return ()

