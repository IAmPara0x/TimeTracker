module Main where

import ArgsUtils
import InfoUtils
import System.Console.ANSI
import System.Environment

main :: IO ()
main = do
  x <- doesInfoFileExist
  if not x
    then createInfoFile
    else do
      setSGR [SetColor Foreground Vivid Blue]
      putStrLn
        "Please enter one of the following command\
        \ \n init   : This command will initialize a time tracker file for today.\
        \ \n add    : will add the current task you are doing in the time tracker file. \
        \ \n commit : will stop today's time tracking. \
        \ \n view   : will open a time tracker file given a date. If not it will open today's file.\
        \ \n exit   : will exit this program."
      setSGR [Reset]
      getLine >>= exec . checkArgs
