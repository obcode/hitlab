{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Logging         (withFileLogging, withStdoutLogging)
import           Create
import           Data.Configurator
import           Data.Configurator.Types (Value (String))
import           Data.Text               (unpack)
import           Datatypes
import           Options.Applicative
import           Prelude                 hiding (lookup)
import           Pull
import           Push

args ::Options.Applicative.Parser Conf
args = Conf
    <$> strOption
        (   long "conf"
         <> short 'c'
         <> value "$(HOME)/.hitlab.conf"
         <> metavar "FILE"
         <> help "Conf FILE"
        )
    <*> cmdParser

cmdParser = subparser $
    command "create" (info createOptions
        (progDesc "Create new repositories for groups"))
    <> command "pull" (info pullOptions
        (progDesc "Pull from all repos, clones if not present"))
    <> command "push" (info pushOptions
        (progDesc "Push current repo into all given repos"))

main :: IO ()
main = do
    conf <- execParser opts
    cfg <- load [Required (conffile conf)]
    maybeLogfile <- lookup cfg "logfile" :: IO (Maybe Value)
    case maybeLogfile of
        Nothing -> withStdoutLogging $ process conf
        Just (String logfile) -> -- putStrLn $ unpack logfile
            withFileLogging (unpack logfile) $ process conf
    -- process conf
    return ()
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
        Push opts   -> push opts

