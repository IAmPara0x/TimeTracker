module ArgsUtils where

import CacheUtils
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Data.Time (LocalTime, getCurrentTimeZone, localDay, utcToLocalTime)
import Data.Time.Calendar (showGregorian)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeOrError)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitSuccess)
import System.IO
import Text.Read (readMaybe)

data Arg
  = Init
  | Add
  | Commit
  | View
  | Exit

checkArgs :: String -> (String, Bool)
checkArgs arg =
  if arg `elem` ["init", "add", "commit", "view", "exit"]
    then (arg, True)
    else (arg, False)

exec :: (String, Bool) -> IO ()
exec (arg, res) =
  if res
    then do
      execArgs (createArg arg)
      getLine >>= exec . checkArgs
    else do
      if null arg
        then getLine >>= exec . checkArgs
        else do
          putStrLn (arg ++ " is not a valid command. Please enter again. \n")
          getLine >>= exec . checkArgs
  where
    createArg x
      | x == "init" = Init
      | x == "add" = Add
      | x == "commit" = Commit
      | x == "view" = View
      | otherwise = Exit

execArgs :: Arg -> IO ()
execArgs Init = initArg
execArgs Add = addArg
execArgs Commit = commitArg
execArgs View = viewArg
execArgs Exit = exitArg

initArg :: IO ()
initArg = do
  strDate <- date
  fileExist <- doesTrackerExist
  if fileExist
    then putStrLn "you have already created today's time tracker file. \n"
    else do
      file <- openFile (strDate ++ "_time_tracker.md") WriteMode
      hPutStrLn file ("### Time spent on: " ++ strDate ++ "\n")
      putStrLn "Please enter your wake up time. Default current time. format (HH:MM 24hr)  eg: 16:56"
      strTime <- getLine >>= parseTime
      hPutStrLn file ("> wakeup Time: " ++ strTime ++ "\n")
      putStrLn "Your today's time tracker file has been intialized successfully. \n"
      writeCacheFile (createCacheFile (strDate ++ "_time_tracker.md") None 0 0 0 0)
      hClose file
  where
    parseTime :: String -> IO String
    parseTime str
      | null str = getLocalTime <&> getTimeToString
      | isValidTime str = return str
      | otherwise = do
        putStrLn ("The given time was not valid. " ++ str ++ " Please enter again. \n")
        getLine >>= parseTime
    isValidTime _ = True

addArg :: IO ()
addArg = do
  putStrLn "Please Describe the current task that you will be doing. \n"
  aName <- getLine
  putStrLn
    "Amoung Which of the three categories you will classify you current task?\
    \ \n 1. Productive \
    \ \n 2. UnProductive \
    \ \n 3. Miscellaneous \
    \ \n Enter one of their respective number. \n"
  aT <- getLine >>= isValidType . parseInt
  fileExist <- doesTrackerExist
  if fileExist
    then do
      cacheF <- readCacheFile >>= return . fromJust
      let fName = getTrackerFilename cacheF
      file <- openFile fName AppendMode
      strTime <- getLocalTime <&> getTimeToString
      hPutStrLn file ("> " ++ aName ++ " : " ++ strTime ++ " \n")
      hClose file
      currIntTime <- getTimeInInt
      cacheF' <- calculateTimeSpent cacheF
      writeCacheFile (cacheF' {activityTime = currIntTime, activityType=aT})
      putStrLn "Your current activity has been successfully added. \n"
    else putStrLn "Today's Tracker does not exist please create one. \n"
  where
    parseInt :: String -> Maybe Int
    parseInt s =
      case (readMaybe s :: Maybe Int) of
        Just i ->
          if i == 1 || i == 2 || i == 3
            then Just i
            else Nothing
        Nothing -> Nothing

    isValidType :: Maybe Int -> IO AType
    isValidType (Just i) = getATypeFromInt i
    isValidType Nothing = do
      putStrLn "Please enter a valid number. \n"
      getLine >>= isValidType . parseInt

    getATypeFromInt :: Int -> IO AType
    getATypeFromInt a
      | a == 1 = return Prod
      | a == 2 = return UnProd
      | a == 3 = return Misc

commitArg :: IO ()
commitArg = do
  fileExist <- doesTrackerExist
  if fileExist
     then do
       cacheF <- readCacheFile <&> fromJust
       deleteCacheFile
       let fName = getTrackerFilename cacheF
       fContent <- readFile fName <&> \x -> [e | e <- lines x, not (null e)]
       removeFile fName
       cacheF' <- calculateTimeSpent cacheF
       let tPTime = intTimeToHrs (getTProdTime cacheF')
       let tUTime = intTimeToHrs (getTUnProdTime cacheF')
       let tMTime = intTimeToHrs (getTMiscTime cacheF')
       let tTimeSpentMsgs = ["> Total Productive Time : " ++ show (fst tPTime) ++ "hrs " ++ show (snd tPTime) ++ "min",
                             "> Total UnProductive Time : " ++ show (fst tUTime) ++ "hrs " ++ show (snd tUTime) ++ "min",
                             "> Total Miscellaneous Time : " ++ show (fst tMTime) ++ "hrs " ++ show (snd tMTime) ++ "min"]
       let fContent' = (head fContent : tTimeSpentMsgs) ++ tail fContent
       file <- openFile fName WriteMode
       wFile file fContent'
       hClose file
       putStrLn "success"
      else putStrLn "Today's Tracker does not exist please create one. To commit today's tracker. \n"
  where
    wFile :: Handle -> [String] -> IO ()
    wFile h [] = hPutStrLn h  ""
    wFile h (x:xs) = (hPutStrLn h (x ++ "\n")) >> wFile h xs


viewArg :: IO ()
viewArg = putStrLn "This is view command. \n"

exitArg :: IO ()
exitArg = putStrLn "exiting !!!" >> exitSuccess

-- helper functions
date :: IO String
date = getLocalTime <&> (showGregorian . localDay)

getLocalTime :: IO LocalTime
getLocalTime = do
  currentZone <- getCurrentTimeZone
  utcToLocalTime currentZone <$> getCurrentTime

timeFormat :: String
timeFormat = "%H:%M"

getTimeToString :: LocalTime -> String
getTimeToString = formatTime defaultTimeLocale timeFormat

getStringToTime :: String -> LocalTime
getStringToTime = parseTimeOrError True defaultTimeLocale timeFormat

doesTrackerExist :: IO Bool
doesTrackerExist = do
  fileExist <- doesCacheFileExist
  if fileExist
    then do
      f <- readCacheFile
      case f of
        Just x -> doesFileExist (getTrackerFilename x)
        Nothing -> return False
    else return False

getTimeInInt :: IO Int
getTimeInInt = do
  currTime <- getCurrentTime
  return (floor $ utctDayTime currTime :: Int)

calculateTimeSpent :: CacheVars -> IO CacheVars
calculateTimeSpent f = getTimeInInt <&> updateTimeSpent f
  where
    updateTimeSpent :: CacheVars -> Int -> CacheVars
    updateTimeSpent cF t
      | getAType cF == Prod = cF {totalProdTime = getTProdTime cF + timeDiff cF t}
      | getAType cF == UnProd = cF {totalUnProdTime = getTUnProdTime cF + timeDiff cF t}
      | getAType cF == Misc = cF {totalMiscTime = getTMiscTime cF + timeDiff cF t}
      | getAType cF == None = cF
      where
        timeDiff f' x =  abs (getATime f' - x)

intTimeToHrs :: Int -> (Int, Int)
intTimeToHrs t = (hrs t, mins t)
  where hrs x = x `div` 3600
        mins x = (x `rem` 3600) `div` 60
