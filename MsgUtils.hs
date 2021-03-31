
module MsgUtils where
import System.Console.ANSI

errMsg :: String -> IO()
errMsg msg = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn msg
  setSGR [Reset]

infoMsg :: String -> IO()
infoMsg msg = do
  setSGR [SetColor Foreground Vivid Yellow]
  putStrLn msg
  setSGR [Reset]

successMsg :: String -> IO()
successMsg msg = do
  setSGR [SetColor Foreground Vivid Green]
  putStrLn msg
  setSGR [Reset]

infoMsgB :: String -> IO()
infoMsgB msg = do
  setSGR [SetColor Foreground Vivid Blue]
  putStrLn msg
  setSGR [Reset]

