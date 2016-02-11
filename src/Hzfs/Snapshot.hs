module Hzfs.Snapshot
    ( Snapshot(..)
    , formatTimestamp
    , parseSnapshots
    , snapshotToText
    ) where

import Hzfs

import Control.Applicative (many)

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (defaultTimeLocale, formatTime, fromGregorian, LocalTime(..), TimeOfDay(..))

import qualified Data.Attoparsec.Text as AP

type TimeFormat = String

timestampFormat :: TimeFormat
timestampFormat = "%F-%T"

formatTimestamp :: TimeFormat -> Snapshot -> String
formatTimestamp format = formatTime defaultTimeLocale format . timestamp

data Snapshot = Snapshot { zpool      :: ZPool
                         , filesystem :: Filesystem
                         , name       :: Name
                         , timestamp  :: LocalTime
                         }
              deriving (Eq)

instance Ord Snapshot where
  s1 `compare` s2 = timestamp s1 `compare` timestamp s2

instance Show Snapshot where
  show s = concat [ Text.unpack $ zpool s
                  , "/", Text.unpack $ filesystem s
                  , "@", Text.unpack $ name s
                  , "-", formatTimestamp timestampFormat s
                  ]

snapshotToText :: Snapshot -> Text
snapshotToText s = Text.concat [ zpool s
                               , Text.singleton '/'
                               , filesystem s
                               , Text.singleton '@'
                               , name s
                               , Text.singleton '-'
                               , Text.pack $ formatTimestamp timestampFormat s
                               ]

isSlash :: Char -> Bool
isSlash c = c == '/'

parseZPool :: AP.Parser ZPool
parseZPool = AP.takeWhile (\c -> isAlphaNum c || c == '-')

parseFilesystem :: AP.Parser Filesystem
parseFilesystem = AP.takeWhile1 (\c -> isAlphaNum c || isSlash c)

parseName :: AP.Parser Name
parseName = AP.takeWhile isAlphaNum

parseTimestamp :: AP.Parser LocalTime
parseTimestamp =
  do y <- AP.count 4 AP.digit
     _ <- AP.char '-'
     m <- AP.count 2 AP.digit
     _ <- AP.char '-'
     d <- AP.count 2 AP.digit
     _ <- AP.char '-'
     h <- AP.count 2 AP.digit
     _ <- AP.char ':'
     mi <- AP.count 2 AP.digit
     _ <- AP.char ':'
     s <- AP.count 2 AP.digit
     return LocalTime { localDay       = fromGregorian (read y) (read m)  (read d)
                      , localTimeOfDay = TimeOfDay     (read h) (read mi) (read s)
                      }

parseSnapshot :: AP.Parser Snapshot
parseSnapshot =
  do p <- parseZPool
     _ <- AP.char '/'
     f <- parseFilesystem
     _ <- AP.char '@'
     n <- parseName
     _ <- AP.char '-'
     t <- parseTimestamp
     AP.endOfLine
     return Snapshot { zpool      = p
                     , filesystem = f
                     , name       = n
                     , timestamp  = t
                     }

parseSnapshots :: Text -> Either String [Snapshot]
parseSnapshots = AP.parseOnly (many parseSnapshot)
