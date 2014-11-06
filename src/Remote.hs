module Remote where

import           Data.List      (isInfixOf, isPrefixOf)
import           System.Process (readProcess)

getRemoteRepoList :: String -> String -> String -> String -> IO [String]
getRemoteRepoList user host port prefix = do
    rawRepoInfo <- readProcess "ssh" [ "-p" ++ port
                                     , user ++ "@" ++ host
                                     , "info"
                                     ] ""
    return $ filter (isPrefixOf prefix)
           $ filter (not . isInfixOf "..*")
           $ map (last . words)
           $ filter (not . null)
           $ dropWhile (not . null)
           $ lines rawRepoInfo
