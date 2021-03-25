module ArgsUtils where

import Text.Read (readMaybe)
import GHC.Generics
import Data.Functor ((<&>))
import Data.Time (LocalTime, UTCTime, getCurrentTimeZone, localDay, utcToLocalTime)
import Data.Time.Calendar (showGregorian)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeOrError)
import System.Directory (doesFileExist)
import System.Exit (exitSuccess)
import System.IO

import CacheUtils


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
      if (null arg)
         then getLine >>= exec . checkArgs
         else putStrLn (arg ++ " is not a valid command. Please enter again. \n")
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
  fileExist <- doesTrackerExist strDate
  if fileExist
    then putStrLn "you have already created today's time tracker file. \n"
    else do
      file <- openFile (strDate ++ "_time_tracker.md") WriteMode
      hPutStrLn file ("### Time spent on: " ++ strDate ++ "\n")
      putStrLn "Please enter your wake up time. Default current time. format (HH:MM 24hr)  eg: 16:56"
      strTime <- getLine >>= parseTime
      hPutStrLn file ("> wakeup Time: " ++ strTime ++ "\n")
      putStrLn "Your today's time tracker file has been intialized successfully. \n"
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
  aNum <- getLine >>= isValidType . parseInt
  strDate <- date
  fileExist <- doesTrackerExist strDate
  if fileExist
    then do
      file <- openFile (strDate ++ "_time_tracker.md") AppendMode
      strTime <- getLocalTime <&> getTimeToString
      hPutStrLn file ("> " ++ aName ++ " : " ++ strTime ++ " \n")
      hClose file
      putStrLn "Your current activity has been successfully added. \n"
    else putStrLn "Today's Tracker does not exist please create one. \n"
  where
    parseInt :: String -> Maybe Int
    parseInt s =
      case (readMaybe s :: Maybe Int) of
        Just i  -> if i == 1 || i == 2 || i == 3
                      then Just i
                      else Nothing
        Nothing -> Nothing

    isValidType :: Maybe Int -> IO Int
    isValidType (Just i) = return i
    isValidType Nothing = do
      putStrLn "Please enter a valid number. \n"
      getLine >>= isValidType . parseInt


commitArg :: IO ()
commitArg = putStrLn "This is commit command. \n"

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
  currentTime <- getCurrentTime
  return (utcToLocalTime currentZone currentTime)

timeFormat :: String
timeFormat = "%H:%M"

getTimeToString :: LocalTime -> String
getTimeToString = formatTime defaultTimeLocale timeFormat

getStringToTime :: String -> LocalTime
getStringToTime = parseTimeOrError True defaultTimeLocale timeFormat

doesTrackerExist :: String -> IO Bool
doesTrackerExist name = doesFileExist (name ++ "_time_tracker.md")

