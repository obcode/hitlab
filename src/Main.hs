{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Csv
import Data.List
import qualified Data.Vector as V
import Options.Applicative

data Conf = Conf
    { conffile :: FilePath
    , cmd :: Cmd
    }
  deriving (Show)

data Cmd = Create CreateOptions
  deriving (Show)

data CreateOptions = CreateOptions
    { prefix :: String
    , template :: FilePath
    }
  deriving (Show)

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
        (progDesc "Print gitolite conf for new repositories"))

createOptions = Create
  <$> (CreateOptions
    <$> strOption
        ( long "prefix"
      <> short 'p'
      <> value "none"
      <> metavar "PREFIX"
      <> help "Use PREFIX for repositories" )
    <*> strOption
        ( long "template"
      <> short 't'
      <> value "none"
      <> metavar "TEMPLATE"
      <> help "Use TEMPLATE for repositories" )
  )

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
process conf = do
    confContents <- BL.readFile $ conffile conf
    let groups = mkGroupsFromFile confContents
    case cmd conf of
        Create opts -> print groups -- $ prefix opts ++ "-"

mkGroupsFromFile :: BL.ByteString -> [(String,[String])]
mkGroupsFromFile contents =
    let myOptions = defaultDecodeOptions {
                decDelimiter = fromIntegral (ord '\t')
                }
    in case decodeWith myOptions HasHeader contents of
        Left _ -> []
        Right v ->
            map ((\(g:_, studs) -> (g, studs)) . unzip)
            $ groupBy (\(grp1,_) (grp2,_) -> grp1 == grp2)
            $ V.toList $ V.map
            (\ (name, firstname, _ :: String, _ :: String, grp ) ->
                        (grp, filter (/=' ') $ firstname ++ name)) v
