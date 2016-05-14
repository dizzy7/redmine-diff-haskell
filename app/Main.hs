module Main where

import Text.Regex.PCRE.Light
import qualified Data.ByteString.UTF8 as B
import Data.Maybe
import Data.List (nub)
import Text.Printf
import qualified System.Process as Proc
import qualified System.Environment as Env

gitExec :: String -> String -> String -> IO String
gitExec path branch1 branch2 = do
    Proc.readProcess "git" ["--git-dir", path ++ "/.git", "--no-pager", "log", "--pretty=oneline", (printf "%s..%s" branch1 branch2)] ""

extractTasks :: [String] -> [String]
extractTasks [] = []
extractTasks (x:xs) = let result = (testPCRELight "#(\\d+)" x) in
                      case result of
                        Just result -> (B.toString (result !! 1)) : extractTasks xs
                        Nothing -> extractTasks xs

formatTasks :: [String] -> [String]
formatTasks [] = []
formatTasks (x:xs) = ("http://redmine/issue/" ++ x) : formatTasks xs

testPCRELight :: String -> String -> Maybe [B.ByteString]
testPCRELight pattern str =
    let r = compile (B.fromString pattern) [utf8] in
    match r (B.fromString str) []

main :: IO ()
main = do
     args <- Env.getArgs
     if length args == 3
        then do
          gitOut <- gitExec (args !! 0) (args !! 1) (args !! 2)
          print $ formatTasks $ nub . extractTasks $ lines gitOut
        else
          putStrLn "Usage: redmine /path/to/git branch1 branch2"



