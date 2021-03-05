module Stamp where

import Data.Time
import Data.Fixed

type Problem = String

type Version = Integer

data Diff = Diff
  { diffContents :: String,
    diffLastUpdatedAt :: UTCTime
  }
  deriving (Show)

data Reflection = Reflection
  { descContents :: String,
    descLastUpdatedAt :: UTCTime
  }
  deriving (Show)

data Language = JavaScript | Haskell | Go | C deriving (Show)

createLanguage :: String -> Language
createLanguage "hs" = Haskell
createLanguage "haskell" = Haskell
createLanguage "js" = JavaScript
createLanguage "JavaScript" = JavaScript
createLanguage _ = JavaScript

lngDir :: Language -> Maybe String
lngDir Haskell = Just "Haskell"
lngDir JavaScript = Just "JavaScript"
lngDir _ = Nothing

lngExt :: Language -> Maybe String
lngExt Haskell = Just "hs"
lngExt JavaScript = Just "js"
lngExt _ = Nothing

data Stamp = Stamp
  { problem :: Problem,
    language :: Language,
    version :: Version,
    diff :: Diff,
    reflection :: Reflection
  }
  deriving (Show)

toUTCTime :: (Integer, Int, Int) -> (Int, Int, Pico) -> UTCTime
toUTCTime (year, mon, day) (hour, min, sec) =
  UTCTime
    (fromGregorian year mon day)
    (timeOfDayToTime (TimeOfDay hour min sec))

diffTimeStamp :: Stamp -> UTCTime
diffTimeStamp stamp = diffLastUpdatedAt $ diff (stamp)

refTimeStamp :: Stamp -> UTCTime
refTimeStamp stamp = descLastUpdatedAt $ reflection (stamp)