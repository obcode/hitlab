-- | Helper functions for accessing the remote site.
module Remote ( getRemoteRepoList ) where

import           Control.Applicative ((<$>))
import           Data.List           (isInfixOf)
import           System.Process      (readProcess)

-- | Get a list of all repos you have access to (determined by your
--   SSH key).
getRemoteRepoList :: String -- ^ The git user on the remote site
                  -> String -- ^ The hostname
                  -> String -- ^ The port
                  -> String -- ^ Get only repos with this string as infix
                  -> IO [String] -- ^ The list of reponames
getRemoteRepoList user host port infixStr =
    getRemoteRepoList' infixStr
        <$> readProcess "ssh" [ "-p" ++ port
                              , user ++ "@" ++ host
                              , "info"
                              ] ""

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
getRemoteRepoList' :: String   -- ^ Get only repos with this string as infix
                   -> String   -- ^ The raw output from the gitolite info cmd
                   -> [String] -- ^ The list of reponames
getRemoteRepoList' infixStr =
           filter (isInfixOf infixStr)
           . filter (not . isInfixOf "..*") -- those are not concrete repos
           . map (last . words)             -- the last word is the repo name
           . takeWhile (not . null)         -- take all 'til the next emptyline
           . dropWhile null                 -- drop empty lines
           . dropWhile (not . null)         -- drop the header
           . lines
