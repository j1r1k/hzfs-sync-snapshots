module Main where

import Hzfs (Filesystem, ZPool)
import Hzfs.Snapshot (Snapshot())
import qualified Hzfs.Snapshot as Snapshot

import Data.EitherR (fmapL)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text.IO as Text (getContents, putStrLn)
import qualified Data.Text as Text (concat, pack, singleton)

import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MultiMap (fromList, lookup, keys)

import Options.Applicative

import Safe (headMay, maximumByMay)

import System.Exit (exitFailure)

sameSnapshot :: Snapshot -> Snapshot -> Bool
sameSnapshot s1 s2 =  Snapshot.filesystem s1 == Snapshot.filesystem s2
                   && Snapshot.name s1       == Snapshot.name s2
                   && Snapshot.timestamp s1  == Snapshot.timestamp s2

type SnapshotMap = MultiMap Filesystem Snapshot

toSnapshotMap :: [Snapshot] -> SnapshotMap
toSnapshotMap = MultiMap.fromList . map (\s -> (Snapshot.filesystem s, s))

lookupNewest :: Filesystem -> SnapshotMap -> Maybe Snapshot
lookupNewest fs = maximumByMay (comparing Snapshot.timestamp) . MultiMap.lookup fs

lookupSame :: Filesystem -> Snapshot -> SnapshotMap -> Maybe Snapshot
lookupSame fs ss = headMay . filter (sameSnapshot ss) . MultiMap.lookup fs

synchronizationPairs :: ZPool -> ZPool -> [Snapshot] -> Either HzfsError [(Snapshot, Snapshot)]
synchronizationPairs src trg snapshots
    | null srcSnapshots = Left  $ InputError "No snapshots in source zpool"
    | null trgSnapshots = Left  $ InputError "No snapshots in target zpool"
    | otherwise                              = Right $ pairs srcSnapshots trgSnapshots
    where withZPool zpool = filter (\s -> zpool == Snapshot.zpool s)
          srcSnapshots = withZPool src snapshots
          trgSnapshots = withZPool trg snapshots

pairs :: [Snapshot] -> [Snapshot] -> [(Snapshot, Snapshot)]
pairs src trg = pairs' (toSnapshotMap src) (toSnapshotMap trg)

pairs' :: SnapshotMap -> SnapshotMap -> [(Snapshot, Snapshot)]
pairs' src trg = mapMaybe (pair src trg) $ MultiMap.keys src

pair :: SnapshotMap -> SnapshotMap -> Filesystem -> Maybe (Snapshot, Snapshot)
pair src trg fs =
  do
    srcNewest <- lookupNewest fs src
    trgNewest <- lookupNewest fs trg
    srcSource <- lookupSame fs trgNewest src
    if srcSource `sameSnapshot` srcNewest then Nothing else return (srcSource, srcNewest)

result :: ZPool -> (Snapshot, Snapshot) -> Text
result trg (f, t) = Text.concat [ Snapshot.snapshotToText f
                    , space
                    , Snapshot.snapshotToText t
                    , space
                    , trg
                    , Text.singleton '/'
                    , Snapshot.filesystem t
                    ]
    where space = Text.singleton ' '

results :: ZPool -> [(Snapshot, Snapshot)] -> [Text]
results trg = map (result trg)


opParseZPool :: Parser ZPool
opParseZPool = Text.pack <$> argument str (metavar "ZPOOL")

data HzfsOptions = HzfsOptions
    { srcZPool :: ZPool
    , trgZPool :: ZPool
    } deriving (Show)

opParseHzfsOptions :: Parser HzfsOptions
opParseHzfsOptions = HzfsOptions <$> opParseZPool
                                 <*> opParseZPool

execOpParserHzfsOptions :: IO HzfsOptions
execOpParserHzfsOptions = execParser $ info (helper <*> opParseHzfsOptions) (fullDesc <> header "Find ZFS synchronization pairs")

data HzfsError = ParseError String
               | InputError String
               deriving (Show)

printError :: HzfsError -> IO ()
printError e = putStrLn $ "Error: " ++ show e

listSnapshots :: Text -> Either HzfsError [Snapshot]
listSnapshots = fmapL ParseError . Snapshot.parseSnapshots

hzfsOptionsHandler :: HzfsOptions -> Text -> Either HzfsError [Text]
hzfsOptionsHandler (HzfsOptions src trg) input =
  do snapshots     <- listSnapshots input
     snapshotPairs <- synchronizationPairs src trg snapshots
     return $ results trg snapshotPairs

main :: IO ()
main =
  do
    options <- execOpParserHzfsOptions
    input   <- Text.getContents
    case hzfsOptionsHandler options input
        of Left err     -> printError err >> exitFailure
           Right output -> mapM_ Text.putStrLn output
