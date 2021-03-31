{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module CacheUtils
  ( CacheVars (..),
    AType (..),
    readCacheFile,
    writeCacheFile,
    deleteCacheFile,
    createCacheFile,
    getTrackerFilename,
    getAType,
    getATime,
    getTProdTime,
    getTUnProdTime,
    getTMiscTime,
    doesCacheFileExist,
  )
where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import System.Directory (doesFileExist, removeFile)
import InfoUtils (infoDirPath)

data CacheVars = CacheVars
  { trackerFileName :: String,
    activityType :: AType,
    activityTime :: Int,
    totalProdTime :: Int,
    totalUnProdTime :: Int,
    totalMiscTime :: Int
  }
  deriving (Show, Generic)

instance FromJSON CacheVars

instance ToJSON CacheVars

data AType
  = Prod
  | UnProd
  | Misc
  | None
  deriving (Eq, Show, Generic)

instance FromJSON AType

instance ToJSON AType

cacheFile :: IO FilePath
cacheFile = infoDirPath >>= \x -> return (x ++ "cache.json")

doesCacheFileExist :: IO Bool
doesCacheFileExist = cacheFile >>= doesFileExist

readCacheFile :: IO (Maybe CacheVars)
readCacheFile = do
  f <- (eitherDecode <$> getJSON) :: IO (Either String [CacheVars])
  case f of
    Left _ -> return Nothing
    Right (ps : _) -> return (Just ps)
    Right [] -> return Nothing
  where
    getJSON :: IO B.ByteString
    getJSON = cacheFile >>= B.readFile 

writeCacheFile :: CacheVars -> IO ()
writeCacheFile cache = cacheFile >>= \x -> B.writeFile x (encode [cache])

deleteCacheFile :: IO ()
deleteCacheFile = cacheFile >>= removeFile 

createCacheFile :: String -> AType -> Int -> Int -> Int -> Int -> CacheVars
createCacheFile = CacheVars

getTrackerFilename :: CacheVars -> String
getTrackerFilename (CacheVars a _ _ _ _ _) = a

getAType :: CacheVars -> AType
getAType (CacheVars _ a _ _ _ _) = a

getATime :: CacheVars -> Int
getATime (CacheVars _ _ a _ _ _) = a

getTProdTime :: CacheVars -> Int
getTProdTime (CacheVars _ _ _ a _ _) = a

getTUnProdTime :: CacheVars -> Int
getTUnProdTime (CacheVars _ _ _ _ a _) = a

getTMiscTime :: CacheVars -> Int
getTMiscTime (CacheVars _ _ _ _ _ a) = a
