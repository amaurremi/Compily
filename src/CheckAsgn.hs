module CheckAsgn (
  -- * Verifies left sides of assignments for not being a function
  --   or procedure
  checkAsgn
  ) where

import Link
import Grammar

import Data.Set hiding (null, map)
import Data.Map hiding (null, map)
import Control.Monad.State

data CheckState = CheckState {  addrNotToBeAssigned :: Set Address
                              , assignments         :: Map Address Ident}

checkAsgn :: (ProgTree LinkId, [ErrMess]) -> [ErrMess]
checkAsgn (tree, msgs) = msgs ++ (getMessages $
  execState (check tree) $ CheckState Data.Set.empty Data.Map.empty)

getMessages :: CheckState -> [ErrMess]
getMessages (CheckState addrSet lnkMap) = map (\id ->
  ("Assignment to rvalue", getPos id)) $
  Data.Map.elems $ filterWithKey (\k _ -> k `Data.Set.member` addrSet) lnkMap

getPos  = fst
getAddr = snd

check :: ProgTree LinkId -> State CheckState ()
check = mapM_ checkDecl

checkDecl :: Decl LinkId -> State CheckState ()
checkDecl (Decl (_, addr) _ body _) = do
  addBadAssign addr
  checkStmt body
checkDecl _                         = return ()

addAssign :: LinkId -> State CheckState ()
addAssign (id, addr) = do
  state <- get
  put $ state {assignments = Data.Map.insert addr id $ assignments state}

addBadAssign :: Address -> State CheckState ()
addBadAssign addr = do
  state <- get
  put $ state {addrNotToBeAssigned = Data.Set.insert addr $
    addrNotToBeAssigned state}

checkElse :: ElseStmt LinkId -> State CheckState ()
checkElse EndIf       = return ()
checkElse (Else stmt) = checkStmt stmt

checkStmt :: Statement LinkId -> State CheckState ()
checkStmt (IfExpr _ cons alt)      = do
  checkStmt cons
  checkElse alt
checkStmt (While _ body)              = checkStmt body
checkStmt (Block stmts)               = mapM_ checkStmt stmts
checkStmt (DeclSt decl)               = checkDecl decl
checkStmt (Assign (Call lnk suffs) _) =
  let addy     = addAssign lnk
  in if null suffs
     then addy
     else case last suffs of
               ArgSuff _ -> do
                             addy
                             addBadAssign $ getAddr lnk
               _         -> return ()
checkStmt _                           = return ()
