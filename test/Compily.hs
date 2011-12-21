import Scanner (gLex)
import Filter (filterKeywords)
import Parser
import Link
import Check
import Translator
import Control.Monad

import System.Environment
import System.Process
import System.Directory
import System.IO
import System.FilePath.Posix (dropExtension)

isNewerThan :: FilePath -> FilePath -> IO Bool
f1 `isNewerThan` f2 = do
  e1 <- doesFileExist f1
  e2 <- doesFileExist f2
  case (e1, e2) of
    (False, _) -> return False
    (True, False) -> return True
    (True, True) -> do
      t1 <- getModificationTime f1
      t2 <- getModificationTime f2
      return $ t1 > t2

compileMS :: String -> String -> String -> String -> IO Bool
compileMS className file java content = do
  putStrLn $ "Compiling " ++ file
  case translate className java $ check file $ linkTree $ createTree 
                 $ filterKeywords $ gLex file content of
       Left error   -> do { hPutStrLn stderr error; return False; }
       Right parsed -> do { parsed; return True; }

compileJava :: String -> IO ()
compileJava java = do
  putStrLn $ "Compiling " ++ java
  exec "javac" ["-classpath", ".;runtimy.jar", java]

exec :: String -> [String] -> IO ()
exec cmd args = do
  h <- runProcess cmd args Nothing Nothing Nothing Nothing Nothing
  waitForProcess h
  return ()

runFile :: String -> String -> IO ()
runFile file content = do
  let className = dropExtension file
  let java = className ++ ".java"
  let bytecode = className ++ ".class"
  shouldCompileMS <- file `isNewerThan` java
  compileOK <- if shouldCompileMS then compileMS className file java content
                                  else return True
  if compileOK then do
      shouldCompileJava <- java `isNewerThan` bytecode
      when shouldCompileJava $ compileJava java
      exec "java" ["-Xmx1024m", "-server", "-classpath", ".;runtimy.jar", className]
    else return ()

main = do
  args     <- getArgs
  preludy  <- readFile "Preludy"
  contents <- mapM readFile args
  mapM_ (uncurry runFile) $ zip args $ map (preludy ++) contents
