-- | Linking identifiers

module Link (
  -- * Takes a syntax tree, returns the same tree with linked identifiers
  linkTree
  -- * Address for linking identifiers
  , Address
  -- * Pair of error message and the source position it refers to
  , ErrMess
  -- * Linked identifier (pair of identifier and its address)
  , LinkId
  -- * Get the string for an identifier
  , showId
  -- * Get the source position of a linked identifier
  , showPos
  ) where

import Grammar

import Prelude hiding (mapM)
import Text.ParserCombinators.Parsec (ParseError, SourcePos)
import Control.Monad.State hiding (mapM)
import Data.Traversable (mapM)
import Text.Parsec.Pos (initialPos)

type Address   = Int
type ErrMess   = (String, SourcePos)
type Context   = [LinkId]
type LinkId    = (Ident, Address)

data ProgState = ProgState {  address :: Address
                            , context :: [Context]
                            , errmess :: [ErrMess]} deriving Show

linkTree ::    Either ParseError (ProgTree Ident)
            -> Either ParseError (ProgTree LinkId, [ErrMess])
linkTree = fmap (linky $ ProgState 0 [[]] [])

linky :: ProgState -> ProgTree Ident -> (ProgTree LinkId, [ErrMess])
linky state progTree =
  let (tree, ProgState _ _ errors) = flip runState state $ addLinks progTree
  in  (tree, errors)

addLinks :: ProgTree Ident -> State ProgState (ProgTree LinkId)
addLinks decls = do
  mapM linkId (filter (not . null . snd) (map (\decl -> case decl of
    Decl id _ _ _ -> id
    _             -> (initialPos "", "")) decls))
  mapM linkGlobalDecl decls

linkAbstractDecl ::    (Ident -> State ProgState LinkId)
                    -> Decl Ident
                    -> State ProgState (Decl LinkId)
linkAbstractDecl linkFunction (Decl id args stmt fp) = do
  lnk    <- linkFunction id
  addContext
  lArgs  <- mapM linkId args
  lStmt  <- linkStmt False stmt
  delContext
  return $ Decl lnk lArgs lStmt fp
linkAbstractDecl _ (VarDecl id val) = do
   lnk   <- linkId id
   lVal  <- mapM linkExpr val
   return $ VarDecl lnk lVal

linkGlobalDecl :: Decl Ident -> State ProgState (Decl LinkId)
linkGlobalDecl = linkAbstractDecl linkGlobalCall

linkDecl :: Decl Ident -> State ProgState (Decl LinkId)
linkDecl = linkAbstractDecl linkId

addContext :: State ProgState ()
addContext = changeCtxt ([]:)

delContext :: State ProgState ()
delContext = changeCtxt tail

changeCtxt f = do
  state <- get
  put $ state {context = f $ context state}

linkStmt :: Bool -> Statement Ident -> State ProgState (Statement LinkId)
linkStmt needsCtxt (Block stmts) = do
  when needsCtxt addContext
  lStmts <- mapM (linkStmt True) stmts
  when needsCtxt delContext
  return $ Block lStmts
linkStmt _ (IfExpr pred conseq alternatives) = do
  lPred  <- linkExpr pred
  lCons  <- do
    addContext
    cons <- linkStmt True conseq
    delContext
    return cons
  lAlt   <- linkElse alternatives
  return $ IfExpr lPred lCons lAlt
linkStmt _ (While cond body) = do
  lCond  <- linkExpr cond
  lBody  <- linkStmt True body
  return $ While lCond lBody
linkStmt _ (CallP call) = do
  lCall  <- linkCall call
  return $ CallP lCall
linkStmt _ (Return expr) = do
  lExpr  <- linkExpr expr
  return $ Return lExpr
linkStmt _ (DeclSt decl) = do
  lDecl  <- linkDecl decl
  return $ DeclSt lDecl
linkStmt _ (Assign var expr) = do
  lVar  <- linkCall var
  lExpr  <- linkExpr expr
  return $ Assign lVar lExpr

linkElse :: ElseStmt Ident -> State ProgState (ElseStmt LinkId)
linkElse EndIf = return EndIf
linkElse (Else stmt) = do
  elseStmt <- linkStmt True stmt
  return $ Else elseStmt

linkExpr :: Expr Ident -> State ProgState (Expr LinkId)
linkExpr (BinOpExpr op expr1 expr2) = do
    e1 <- linkExpr expr1
    e2 <- linkExpr expr2
    return $ BinOpExpr op e1 e2
linkExpr (UnOpExpr op unexpr)       = do
  e  <- linkExpr unexpr
  return $ UnOpExpr op e
linkExpr (Table maps)               = do
  let (keys, vals) = unzip maps
  keys' <- mapM linkExpr keys
  vals' <- mapM linkExpr vals
  return $ Table $ zip keys' vals'
linkExpr (ElemPrim el)              = return . ElemPrim =<< linkCall el
linkExpr (Str str)                  = return $ Str str
linkExpr (Num num)                  = return $ Num num
linkExpr (Bln bln)                  = return $ Bln bln

linkId :: Ident -> State ProgState LinkId
linkId id@(pos, name) = do
  state@(ProgState addr (c : cs) err) <- get
  let lnk = (id, addr)
  lookUpId name c
    (do
      put state {  address = addr + 1
                 , context = (lnk : c) : cs}
      return lnk)
    (\_ -> do
             put state {errmess = (showId id
                ++ " already defined in this scope.", pos) : err}
             return (id, -1))

lookUpId ::    String 
            -> Context
            -> State ProgState a
            -> (Address -> State ProgState a)
            -> State ProgState a
lookUpId str ctxt ifNothing f = maybe ifNothing f $ lookup str
  $ map (\((_, str), addr) -> (str, addr)) ctxt

linkAbstractCall ::    Ident
                    -> [Suffix Ident]
                    -> State ProgState (LinkId, [Suffix LinkId])
linkAbstractCall id@(pos, name) suffs = do
  lnk      <- do
    state@(ProgState _ ctxt err) <- get
    lookUpId
      name
      (concat ctxt)
      (do
        put state {errmess = (showId id ++ " not defined.", pos) : err}
        return (id, -1))
      (return . (,) id)
  lnksuffs <- mapM linkSuff suffs
  return (lnk, lnksuffs)

linkCall :: Call Ident -> State ProgState (Call LinkId)
linkCall (Call id suffixes) = do
  (lnk, lnksuffs) <- linkAbstractCall id suffixes
  return $ Call lnk lnksuffs

linkGlobalCall :: Ident -> State ProgState LinkId
linkGlobalCall id = do
  (lnk, _) <- linkAbstractCall id []
  return lnk

linkSuff :: Suffix Ident -> State ProgState (Suffix LinkId)
linkSuff (ArgSuff exprs) = do
  lexprs  <- mapM linkExpr exprs
  return $ ArgSuff lexprs
linkSuff (KeySuff key) = do
  lkey <- linkExpr key
  return $ KeySuff lkey

showId :: Ident -> String
showId (pos, name) = show pos ++ ": Identifier '" ++ name ++ "'"

showPos = fst . fst
