{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Logging         (withFileLogging, withStdoutLogging)
import           Data.Configurator
import           Data.Configurator.Types (Value (String))
import           Data.Text               (Text, unpack)
import           Options.Applicative
import           Prelude                 hiding (lookup)

import           Create
import           Datatypes
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

addDefaultsFromConffile :: Conf -> IO RichConf
addDefaultsFromConffile conf = do
    cfg <- load [Optional (conffile conf)]
    maybeLogfile  <- ((\(String s) -> s) <$>)
                     <$> lookup cfg "logfile"
    maybeRepoPrg <- ((\(String s) -> s) <$>)
                     <$> lookup cfg "repo_prg"
    return $ RichConf conf maybeLogfile maybeRepoPrg

main :: IO ()
main = do
    cmdConf <- execParser opts
    richconf <- addDefaultsFromConffile cmdConf
    maybe withStdoutLogging (withFileLogging . unpack) (maybeLogfile richconf)
        $ process richconf
  where
    opts = info (helper <*> args)
      ( fullDesc
     <> progDesc "manage git repos for student tasks"
     <> header "hitlab - Haskell Git for LAB " )

process :: RichConf -> IO ()
process richconf =
    case cmd $ conf richconf of
        Create opts -> mkRepoEntries opts
        Pull opts   -> pull richconf opts
        Push opts   -> push opts

