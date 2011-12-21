module CheckMain (
  -- * Checks the presence of a main() procedure
  checkMain
  ) where

import Grammar
import Link

import Text.Parsec.Pos (initialPos)

checkMain :: FilePath -> (ProgTree LinkId, [ErrMess]) -> [ErrMess]
checkMain source (tree, msgs) = msgs ++
  (if hasMainProcedure tree
    then []
    else [("No main() procedure.", initialPos source)])

hasMainProcedure :: ProgTree LinkId -> Bool
hasMainProcedure = any isMainProcedure

isMainProcedure :: Decl LinkId -> Bool
isMainProcedure (VarDecl _ _) = False
isMainProcedure (Decl ((_, name), _) args _ isFunction) = 
  name == "main" && not isFunction && null args
