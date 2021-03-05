module Lib
  ( stamp,
  )
where

import Data.Time
import qualified Database.PostgreSQL.Simple as SQL
import qualified Db as Db
import Stamp
import System.Directory
import System.IO

data Command = Command
  { problemCmd :: String,
    languageCmd :: Language
  }

type ProblemPaths = (FilePath, FilePath)

type ProblemTimeStamp = (UTCTime, UTCTime)

{--
stamp
- getProblemFilePaths -> problemFileCheck -> getLatestStamp -> Yes or No
  ["yes" constructNewStampFromExistingStamp | "no" constructNewStamp ]

constructNewStampFromExistingStamp
- resolveNewTimeStamps* -> constructStamp (+1 version)

constructNewStamp
- getTimeStamps -> constructStamp (1 version)
--}

stamp :: SQL.Connection -> Command -> IO (Either String String)
stamp conn cmd = do
  let paths = getProblemFilePaths cmd
  newStampRes <- case paths of
    Just (pPath, dPath) -> do
      fileCheck <- problemFileCheck (pPath, dPath)

      -- Check if files for problem exisit
      case fileCheck of
        Right _ -> do
          latestStamp <- Db.getLatestStamp conn (problemCmd cmd) (languageCmd cmd)
          stampRes <- case latestStamp of
            -- create new with comparing old and new timestamps
            Just lstamp -> do
              res <- makeStampFromExistingProb cmd lstamp (pPath, dPath)
              return res
            -- create new stamp without latest times stamps
            Nothing -> do
              res <- makeStampAsStart cmd (pPath, dPath)
              return res
          return $ stampRes
        Left err -> return (Left err)
    Nothing -> return $ Left "error: lanugage not supported"

  -- Add new stamp to Db
  case newStampRes of
    Right stamp -> do
      Db.addStamp conn stamp
      return $ Right "Stamp added"
    Left err -> return $ Left err

-- makeStampFromExistingProb
makeStampFromExistingProb ::
  Command ->
  Stamp ->
  ProblemPaths ->
  IO (Either String Stamp)
makeStampFromExistingProb cmd lstamp (pPath, dPath) = do
  timeStamps <-
    getProblemTimeStamps
      (diffTimeStamp lstamp, refTimeStamp lstamp)
      (pPath, dPath)
  res <- case timeStamps of
    Right pTimeStamps -> do
      newStamp <-
        createNewStamp
          (problemCmd cmd)
          (languageCmd cmd)
          ((version lstamp) + 1)
          pTimeStamps
          (pPath, dPath)
      return $ newStamp
    Left err -> return $ Left err
  return res

-- makeStamp
makeStampAsStart ::
  Command ->
  ProblemPaths ->
  IO (Either String Stamp)
makeStampAsStart cmd (pPath, dPath) = do
  diffTimeStamp <- getModificationTime pPath
  reflectionTimeStamp <- getModificationTime dPath
  newStamp <-
    createNewStamp
      (problemCmd cmd)
      (languageCmd cmd)
      1
      (diffTimeStamp, reflectionTimeStamp)
      (pPath, dPath)
  return $ newStamp

createNewStamp ::
  Problem ->
  Language ->
  Version ->
  ProblemTimeStamp ->
  ProblemPaths ->
  IO (Either String Stamp)
createNewStamp
  prob
  lng
  ver
  (pTimeStamp, rTimeStamp)
  (diffPath, reflectionPath) =
    do
      diffRes <- readFile diffPath
      reflectionRes <- readFile reflectionPath
      let pdiff = Diff diffRes pTimeStamp
      let rConetents = Reflection reflectionRes rTimeStamp
      return $ (Right (Stamp prob lng ver pdiff rConetents))

-- problemFileCheck: Returns handlers if problem files exsist
problemFileCheck :: ProblemPaths -> IO (Either String String)
problemFileCheck (problemPath, descriptionPath) = do
  problemExists <-
    doesFileExist problemPath
  descriptionExists <- doesFileExist descriptionPath
  case (problemExists, descriptionExists) of
    (True, True) ->
      return $ Right "files exist"
    (True, False) ->
      return $ Left "error: descritpion not found"
    (False, True) ->
      return $ Left ("error: problem " ++ "[" ++ problemPath ++ "]" ++ " not found")
    (False, False) ->
      return $ Left ("error: problem " ++ "[" ++ problemPath ++ "]" ++ " not found")

getProblemFilePaths :: Command -> Maybe ProblemPaths
getProblemFilePaths cmd =
  case getProblemPath cmd of
    Just pPath ->
      case getDescriptionPath cmd of
        Just dPath -> Just ((rootPath ++ pPath, rootPath ++ dPath))
        Nothing -> Nothing
    Nothing -> Nothing

getProblemPath :: Command -> Maybe FilePath
getProblemPath cmd =
  case getBasePath cmd of
    Just base ->
      case problemFileName cmd of
        Just filename -> Just $ base ++ filename
        Nothing -> Nothing
    Nothing -> Nothing

getDescriptionPath :: Command -> Maybe FilePath
getDescriptionPath cmd =
  case getBasePath cmd of
    Just base -> Just $ base ++ descriptionFileName
    Nothing -> Nothing

getBasePath :: Command -> Maybe FilePath
getBasePath cmd =
  case lngDir (languageCmd cmd) of
    Just dirName ->
      Just
        ( problemCmd cmd
            ++ "/"
            ++ dirName
            ++ "/"
        )
    Nothing -> Nothing

problemFileName :: Command -> Maybe FilePath
problemFileName cmd =
  case lngExt (languageCmd cmd) of
    Just ext -> Just (problemCmd cmd ++ "." ++ ext)
    Nothing -> Nothing

descriptionFileName :: FilePath
descriptionFileName = "reflection.md"

-- getProblemTimeStamps: ensure modification has happended on both files
-- and returns new timeStamps
getProblemTimeStamps ::
  ProblemTimeStamp ->
  ProblemPaths ->
  IO (Either String ProblemTimeStamp)
getProblemTimeStamps (pLatestTime, dLatestTime) (probPath, descPath) = do
  probTimeStamp <- getTimeStamp pLatestTime probPath
  descTimeStamp <- getTimeStamp dLatestTime descPath
  print descTimeStamp
  case (probTimeStamp, descTimeStamp) of
    (Just probt, Just desct) -> return $ (Right (probt, desct))
    (Nothing, Just desct) -> return $ (Right (pLatestTime, desct))
    (Just probt, Nothing) ->
      return $ (Left "error: description not updated to stamp changes")
    (Nothing, Nothing) ->
      return $ (Left "error: no updates found on problem, update to stamp")

getTimeStamp :: UTCTime -> FilePath -> IO (Maybe UTCTime)
getTimeStamp latest path = do
  lastModificationOnFile <- getModificationTime path
  let hasNewUpdate = latest < lastModificationOnFile
  if hasNewUpdate
    then return $ Just lastModificationOnFile
    else return Nothing

exampleProblem =
  Command
    { problemCmd = "add_one_plus_one",
      languageCmd = Haskell
    }

exampleProblemEr =
  Command
    { problemCmd = "add_one_plus_on",
      languageCmd = Haskell
    }

exampleProblem2 =
  Command
    { problemCmd = "add_one_plus_one",
      languageCmd = JavaScript
    }

rootPath = "./example-stamp/"