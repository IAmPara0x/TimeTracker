{-# LANGUAGE DeriveGeneric #-}

module InfoUtils
  ( createInfoFile,
    doesInfoFileExist,
    readInfoFile,
    writeInfoFile,
    getTrackerDir,
    getTrackerViewer,
    infoDirPath
  )
where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as B
import Data.Functor ((<&>))
import GHC.Generics
import MsgUtils
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory)

data InfoVars = InfoVars
  { trackerDir :: String,
    viewApp :: String
  }
  deriving (Show, Generic)

instance FromJSON InfoVars

instance ToJSON InfoVars

basePath :: IO String
basePath = getCurrentDirectory <&> getPath 0
  where
    getPath :: Int -> [Char] -> [Char]
    getPath acc (x : xs)
      | acc < 3 =
        if x == '/'
          then x : getPath (acc + 1) xs
          else x : getPath acc xs
      | otherwise = []

infoDirPath :: IO String
infoDirPath = basePath >>= \x -> return (x ++ ".timeTrackerCache/")

infoFile :: IO String
infoFile = infoDirPath >>= \x -> return (x ++ "info.json")

doesInfoFileExist :: IO Bool
doesInfoFileExist = infoFile >>= doesFileExist

createInfoVars :: FilePath -> String -> InfoVars
createInfoVars = InfoVars

readInfoFile :: IO (Maybe InfoVars)
readInfoFile = do
  f <- (eitherDecode <$> getJSON) :: IO (Either String [InfoVars])
  case f of
    Left _ -> return Nothing
    Right (ps : _) -> return (Just ps)
    Right [] -> return Nothing
  where
    getJSON :: IO B.ByteString
    getJSON = infoFile >>= B.readFile

writeInfoFile :: InfoVars -> IO ()
writeInfoFile info = do
  x <- infoFile
  B.writeFile x (encode [info])

getTrackerDir :: InfoVars -> FilePath
getTrackerDir (InfoVars f _) = f

getTrackerViewer :: InfoVars -> String
getTrackerViewer (InfoVars _ a) = a

createInfoFile :: IO ()
createInfoFile = do
  infodirExist <- infoDirPath >>= doesDirectoryExist
  infoDirPath >>= createDirectoryIfMissing infodirExist

  infoMsgB
    "This is Ypur first time running this program. \n \
    \ Please specify an absoute path of the directory, where you want to store your time trackers. \n \
    \ Default ($HOME/Desktop/timeTrackers). \n"

  tPath <- getLine >>= getPath
  doesDirectoryExist tPath >>= \b -> createDirectoryIfMissing b tPath

  infoMsgB "Specify the app name that you want to view your tracker with. (It must be on your path)"
  appN <- getLine
  writeInfoFile (createInfoVars tPath appN)
  successMsg "Now you can create the trackers."
  where
    getPath p
      | null p = basePath >>= \p -> return (p ++ "Desktop/timeTrackers/")
      | otherwise = return p
