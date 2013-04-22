-- |
-- Main module of DL1 compiler
-- 
-- The 'dl' command takes an MS (Marianna Script) program written in
-- the DL1 language and translates it into a Java program

import Scanner 
import Filter (filterKeywords)
import Parser
import Link
import Check
import Translator

import System.Environment
import System.IO

main = do
  args     <- getArgs
  preludy  <- readFile "Preludy"
  contents <- mapM readFile args
  mapM_ translateToJava $ zip args $ map (preludy ++) contents
    where translateToJava (arg, content) =
           case translate arg $ check arg $ linkTree $ createTree
                          $ filterKeywords $ gLex arg content of
                Left error   -> hPutStrLn stderr error
                Right parsed -> parsed

