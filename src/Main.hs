module Main where

import           Options.Applicative

import           Create
import           Datatypes
import           Pull

args ::Options.Applicative.Parser Conf
args = Conf
    <$> strOption
        (   long "conf"
         <> short 'c'
         <> value "hitlab.conf"
         <> metavar "FILE"
         <> help "Conf FILE"
        )
    <*> cmdParser

cmdParser = subparser $
    command "create" (info createOptions
        (progDesc "Create new repositories for groups"))
    <> command "pull" (info pullOptions
        (progDesc "Pull all repos master branch, clones if not present"))

main :: IO ()
main = do
    conf <- execParser opts
    process conf
  where
    opts = info (helper <*> args)
      ( fullDesc
     <> progDesc "manage git repos for student tasks"
     <> header "hitlab - Haskell Git for LAB " )

process :: Conf -> IO ()
process conf =
    case cmd conf of
        Create opts -> mkRepoEntries opts
        Pull opts   -> pull opts

