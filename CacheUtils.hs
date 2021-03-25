{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module CacheUtils
(
CacheVars(..),
AType(..),
readCacheFile,
writeCacheFile,
createCacheFile,
getAType,
getATime,
getTProdTime,
getTUnProdTime,
getTMiscTime
)
where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import GHC.Generics
import qualified Data.ByteString.Lazy as B


data CacheVars = CacheVars
  {
    activityType :: AType,
    activityTime :: Int,
    totalProdTime :: Int,
    totalUnProdTime :: Int,
    totalMiscTime :: Int
  } deriving (Show,Generic)

instance FromJSON CacheVars
instance ToJSON CacheVars

data AType
  = Prod
  | UnProd
  | Misc
  deriving (Show,Generic)

instance FromJSON AType
instance ToJSON AType

cacheFile :: FilePath
cacheFile = "cache.json"

readCacheFile :: IO (Maybe CacheVars)
readCacheFile = do
  f <- (eitherDecode <$> getJSON) :: IO (Either String [CacheVars])
  case f of
    Left _ -> return Nothing
    Right (ps:_) -> return (Just ps)
    Right [] -> return Nothing
    where
      getJSON :: IO B.ByteString
      getJSON = B.readFile cacheFile

writeCacheFile :: CacheVars -> IO()
writeCacheFile cache = B.writeFile cacheFile (encode [cache])

createCacheFile :: AType -> Int -> Int -> Int -> Int -> CacheVars
createCacheFile = CacheVars

getAType :: CacheVars -> AType
getAType (CacheVars a _ _ _ _) = a

getATime :: CacheVars -> Int
getATime (CacheVars _ a _ _ _) = a

getTProdTime :: CacheVars -> Int
getTProdTime (CacheVars _ _ a _ _) = a

getTUnProdTime :: CacheVars -> Int
getTUnProdTime (CacheVars _ _ _ a _) = a

getTMiscTime :: CacheVars -> Int
getTMiscTime (CacheVars _ _ _ _ a) = a

