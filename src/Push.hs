{-# LANGUAGE ScopedTypeVariables #-}
module Pull where

import qualified Data.ByteString.Lazy as BL
import           Data.Char
import           Data.Csv
import           Data.Function        (on)
import           Data.List            (groupBy, intercalate, isInfixOf,
                                       isPrefixOf, sortBy)
import qualified Data.Vector          as V
import           Options.Applicative
import           System.Directory     (doesDirectoryExist)
import           System.IO            (hClose, hGetContents)
import           System.Process       (StdStream (CreatePipe), createProcess,
                                       cwd, proc, readProcess, std_err, std_out,
                                       waitForProcess)

import           Datatypes

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
    rawRepoInfo <- readProcess "ssh" [ "-p" ++ pullPort opts
                                     , pullUser opts ++ "@" ++ pullHost opts
                                     , "info"
                                     ] ""
    let repos = filter (isPrefixOf $ pullPrefix opts)
                $ filter (not . isInfixOf "..*")
                $ map (last . words)
                $ filter (not . null)
                $ dropWhile (not . null)
                $ lines rawRepoInfo
    -- putStrLn $ "Pulling: " ++ show repos
    mapM_ (pullRepo opts) repos

pullRepo :: PullOptions -> FilePath -> IO ()
pullRepo opts repo = do
    alreadyExist <- doesDirectoryExist repo
    if not alreadyExist
    then
      do putStrLn $ "Cloning\x1b[34m " ++ repo ++ "\x1b[0m"
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
      do putStrLn $ "Pulling\x1b[34m " ++ repo ++ "\x1b[0m"
         (_, Just hout, _, ph) <- createProcess
            (proc "git" ["pull", "origin", pullBranch opts])
                            { cwd = Just repo
                            , std_out = CreatePipe
                            , std_err = CreatePipe }
         pullInfo <- hGetContents hout
         putStrLn $ "\x1b[32m" ++ pullInfo ++ "\x1b[0m"
         waitForProcess ph
         return ()

