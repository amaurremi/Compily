module CheckReturn (
  -- * Checks whether function returns anything
  checkReturn
  -- * 
  , nrLastStmt
  -- *
  , nrLastElse
  ) where

import Grammar
import Link

checkReturn :: (ProgTree LinkId, [ErrMess]) -> [ErrMess]
checkReturn (tree, msgs) = msgs ++
  (let noReturnIds = noReturn tree
   in if null noReturnIds
     then []
     else map (\lnk -> (showId (fst lnk)
            ++ " does not end with a return", showPos lnk)) noReturnIds)

noReturn :: ProgTree LinkId -> [LinkId]
noReturn = concatMap nrDecl

nrDecl :: Decl LinkId -> [LinkId]
nrDecl (VarDecl _ _)                = []
nrDecl (Decl lnk _ body isFunction) = 
  (if isFunction && nrLastStmt body then [lnk] else [])
    ++ (nrStmt body)

nrLastStmt :: Statement LinkId -> Bool
nrLastStmt (Return _)          = False
nrLastStmt (Block stmts)       = if null stmts
  then False
  else nrLastStmt $ last stmts
nrLastStmt (IfExpr _ cons alt) = nrLastStmt cons || nrLastElse alt
nrLastStmt _                   = True

nrLastElse :: ElseStmt LinkId -> Bool
nrLastElse EndIf       = False
nrLastElse (Else stmt) = nrLastStmt stmt

nrStmt :: Statement LinkId -> [LinkId]
nrStmt stmt = case stmt of
  IfExpr _ cons alt -> let nrCons = nrStmt cons
    in case alt of
      Else elseStmt   -> nrStmt elseStmt ++ nrCons
      EndIf           -> nrCons
  Block stmts       -> concatMap nrStmt stmts
  While _ body      -> nrStmt body
  DeclSt decl       -> nrDecl decl
  _                 -> []
