{-# LANGUAGE ScopedTypeVariables #-}
module Create where

import qualified Data.ByteString.Lazy as BL
import           Data.Char
import           Data.Csv
import           Data.Function        (on)
import           Data.List            (sortBy, groupBy)
import qualified Data.Vector          as V
import           Options.Applicative

import           Datatypes

createOptions = Create
  <$> (CreateOptions
    <$> strOption
        ( long "prefix"
      <> short 'p'
      <> metavar "PREFIX"
      <> help "Use PREFIX for repositories" )
    <*> strOption
        ( long "desc"
      <> short 'd'
      <> metavar "DESCRIPTION"
      <> help "Use DESCRIPTION for repositories" )
    <*> strOption
        ( long "inputFilename"
      <> short 'f'
      <> metavar "FILENAME"
      <> help "Read moodle groups from FILENAME" )
    <*> strOption
        ( long "output"
      <> short 'o'
      <> value "gitolite.conf"
      <> metavar "FILENAME"
      <> help "write gitolite conf to FILENAME" )
    <*> switch
        ( long "readonly"
      <> short 'r'
      <> help "no write access for students" )
  )

mkRepoEntries :: CreateOptions -> IO ()
mkRepoEntries opts = do
    content <- BL.readFile $ createInfile opts
    let groups = mkGroupsFromFile content
    let repoEntries = mkRepoEntries' (createPrefix opts) (createDesc opts) groups
                                     (createReadOnly opts)
    writeFile (createOutfile opts) repoEntries

mkGroupsFromFile :: BL.ByteString -> [Group]
mkGroupsFromFile contents =
    let myOptions = defaultDecodeOptions {
                decDelimiter = fromIntegral (ord '\t')
                }
        mkGroups = map (uncurry Group)
        realGroup (Group "Nicht abgestimmt" _) = False
        realGroup _ = True
    in case decodeWith myOptions HasHeader contents of
        Left _ -> []
        Right v ->
            sortBy (compare `on`groupname)
            $ filter realGroup
            $ mkGroups
            $ map ((\(g:_, studs) -> (g, studs)) . unzip)
            $ groupBy (\(grp1,_) (grp2,_) -> grp1 == grp2)
            $ V.toList $ V.map
            (\ (name, firstname, _ :: String, _ :: String, grp ) ->
                        (grp, filter (/=' ') $ firstname ++ name)) v

mkRepoEntries' :: String -> String -> [Group] -> Bool -> String
mkRepoEntries' prefix desc groups readonly =
    let mkRepoEntry (Group name members) =
               "repo " ++ prefix ++ "-" ++ name ++ "\n"
            ++ "    - feedback = " ++ unwords members ++ "\n"
            ++ (if readonly
                   then "    R          = "
                   else "    RW         = "
               ) ++ unwords members ++ "\n"
            ++ "    desc       = " ++ desc ++ "\n\n"
    in concatMap mkRepoEntry groups
