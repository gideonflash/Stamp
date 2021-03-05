{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Db where

import Data.Time
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.Time
import Stamp

type StampRow = (Int, Version, String, String, UTCTime, String, UTCTime, String)

type StamRowInsert = (Version, String, String, UTCTime, String, UTCTime, String)

credentials :: SQL.ConnectInfo
credentials =
  SQL.defaultConnectInfo
    { SQL.connectUser = "gideonkadzura",
      SQL.connectDatabase = "stampy"
    }

-- Setting connection
connectDb :: IO SQL.Connection
connectDb = SQL.connect credentials

-- Get latest stamp
getLatestStamp :: SQL.Connection -> Problem -> Language -> IO (Maybe Stamp)
getLatestStamp conn problem language = do
  (row :: [StampRow]) <- SQL.query conn selectLatest (problem, (show language))
  case row of
    [] -> return Nothing
    [r] -> return $ Just (fromRow r)

fromRow :: StampRow -> Stamp
fromRow
  (_, ver, prob, ref, refUpdateAt, diff, diffUpdateAt, lng) =
    Stamp
      { problem = prob,
        version = ver,
        diff = Diff diff diffUpdateAt,
        reflection = Reflection ref refUpdateAt,
        language = createLanguage lng
      }

toRow :: Stamp -> StamRowInsert
toRow
  Stamp
    { problem = prob,
      version = ver,
      diff =
        Diff
          { diffContents = diffContent,
            diffLastUpdatedAt = diffUpdateAt
          },
      reflection =
        Reflection
          { descContents = refContent,
            descLastUpdatedAt = refUpdateAt
          },
      language = lng
    } =
    (ver, prob, refContent, refUpdateAt, diffContent, diffUpdateAt, (show lng))

selectLatest :: SQL.Query
selectLatest = "SELECT * From stamps WHERE problem = ? AND language = ? ORDER BY version DESC LIMIT 1"

-- Add stamp
addStamp :: SQL.Connection -> Stamp -> IO ()
addStamp conn stamp = do
  print stamp
  res <- SQL.execute conn insertStamp (toRow stamp)
  return ()

insertStamp :: SQL.Query
insertStamp = "INSERT INTO stamps (version, problem, description, description_updated_at, diff, diff_update_at, language) VALUES (?, ?, ?, ?, ?, ?, ?)"

exampleStamp :: Stamp
exampleStamp =
  Stamp
    { problem = "some other one",
      version = 1,
      diff = Diff "add = 1 + 5" (toUTCTime (2020, 9, 1) (15, 13, 0)),
      reflection = Reflection "ref ref" (toUTCTime (2020, 9, 1) (15, 13, 0)),
      language = Haskell
    }