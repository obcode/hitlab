-- | Helper functions for accessing the remote site.
module Remote where

import           Data.List      (isInfixOf)
import           System.Process (readProcess)

-- | Get a list of all repos you have access to (determined by your
--   SSH key).
getRemoteRepoList :: String -- ^ The git user on the remote site
                  -> String -- ^ The hostname
                  -> String -- ^ The port
                  -> String -- ^ Get only repos with this string as infix
                  -> IO [String] -- ^ The list of reponames
getRemoteRepoList user host port infixStr = do
    -- Output is something like:
    --
    -- hello obraun, this is git@gitolite ..
    --
    --  R W	braun/14WS/algdatI/testing
    --  R W	braun/14WS/compiler/testing
    --  R W	braun/14WS/sweng/testing
    --  R W	testing
    --
    -- More Infos ...
    rawRepoInfo <- readProcess "ssh" [ "-p" ++ port
                                     , user ++ "@" ++ host
                                     , "info"
                                     ] ""
    return $ filter (isInfixOf infixStr)
           $ filter (not . isInfixOf "..*") -- those are not concrete repos
           $ map (last . words)             -- the last word is the repo name
           $ takeWhile (not . null)         -- take all 'til the next emptyline
           $ dropWhile null                 -- drop empty lines
           $ dropWhile (not . null)         -- drop the header
           $ lines rawRepoInfo
