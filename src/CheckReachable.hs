module CheckReachable (
  -- * Makes sure every statement of a function or procedure is reachable  
  checkReachable
  ) where

import Grammar
import Link
import CheckReturn (nrLastStmt, nrLastElse)

checkReachable :: (ProgTree LinkId, [ErrMess]) -> [ErrMess]
checkReachable (tree, msgs) = msgs ++
  (map (\lnk -> (  showId (fst lnk) ++ " contains unreachable statements"
                        , showPos lnk)) $ getNotReachable tree)

getNotReachable :: ProgTree LinkId -> [LinkId]
getNotReachable = concatMap nrDecl

nrDecl :: Decl LinkId -> [LinkId]
nrDecl (VarDecl _ _)       = []
nrDecl (Decl lnk _ body _) = nrStmt lnk body

nrStmt :: LinkId -> Statement LinkId -> [LinkId]
nrStmt lnk (Block body)        = checkReach lnk body
nrStmt lnk (IfExpr _ cons alt) = nrStmt lnk cons ++ (nrElse lnk alt)
nrStmt lnk (While _ body)      = nrStmt lnk body
nrStmt _ (DeclSt decl)         = nrDecl decl
nrStmt _ _                     = []

nrElse _ EndIf         = []
nrElse lnk (Else stmt) = nrStmt lnk stmt

checkReach :: LinkId -> [Statement LinkId] -> [LinkId]
checkReach _ []             = []
checkReach lnk (stmt:stmts) = if null stmts
  then nrStmt lnk stmt
  else let checkRest = checkReach lnk stmts
           thisLink  = [lnk]
       in case stmt of
            Return _                      -> thisLink
            IfExpr _ cons alt             ->
              if not (nrLastStmt cons || nrLastElse alt)
              then thisLink
              else nrStmt lnk cons ++ (case alt of
                    EndIf     -> []
                    Else stmt -> nrStmt lnk stmt)
                      ++ checkRest
            While _ body                  -> nrStmt lnk body ++ checkRest
            Block block                   -> if containsReturn block
              then thisLink
              else checkReach lnk block ++ checkRest
            DeclSt decl@(Decl _ _ _ _) -> nrDecl decl ++ checkRest
            _                          -> checkRest

containsReturn :: [Statement LinkId] -> Bool
containsReturn = any (\stmt -> case stmt of
  Return _ -> True
  _        -> False)
