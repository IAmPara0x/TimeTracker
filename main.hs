
module Main where
import System.Environment
import ArgsUtils


main :: IO()
main = do
  putStrLn "Please enter one of the following command\
        \ \n init   : This command will initialize a time tracker file for today.\
        \ \n add    : will add the current task you are doing in the time tracker file. \
        \ \n commit : will stop today's time tracking. \
        \ \n view   : will open a time tracker file given a date. If not it will open today's file.\
        \ \n exit   : will exit this program."

  getLine >>= exec . checkArgs


