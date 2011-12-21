module Parser (
  -- | Accepts a list of scanned and filtered tokens and creates a syntax tree
  createTree
  ) where

import Filter (PosToken, DKeyword(..))
import qualified Scanner (GSign(..))
import AuxFuncs
import Grammar

import Prelude hiding (sum)
import Text.ParserCombinators.Parsec hiding (string)

createTree :: Either ParseError [PosToken]
           -> Either ParseError (ProgTree Ident)
createTree (Left error)    = Left error
createTree (Right scanned) = parse (many decl) "sourcename" scanned

decl :: GenParser PosToken st (Decl Ident)
decl =
    let decl forp = do
         fp     <- parseKeyword forp
         name   <- ident
         params <- parameters ident
         stmt   <- statement
         return $ Decl name params stmt (fp == FuncKw)
    in choice (map decl [FuncKw, ProcKw])
  <|> vardecl
  <?> "function, procedure declaration"

vardecl :: GenParser PosToken st (Decl Ident)
vardecl = do
    parseKeyword VarKw
    vname <- ident
    val   <- optionMaybe (s_assign >> expression)
    s_semic
    return $ VarDecl vname val
  <?> "variable declaration"

parameters :: GenParser PosToken st f -> GenParser PosToken st [f]
parameters parForm = do
    s_lpar
    ids <- sepBy parForm s_comma
    s_rpar
    return ids
  <?> "function or procedure parameters"

block = do
    s_lcurly
    stats <- many statement
    s_rcurly
    return (Block stats)
  <?> "block of statements"

statement :: GenParser PosToken st (Statement Ident)
statement = choice
  [ifexpr, while, assign, callp, returnV, declSt, block]

declSt :: GenParser PosToken st (Statement Ident)
declSt = return . DeclSt =<< decl

operations = [s_or, s_and
  , choice [s_equals, s_noteq, s_lt, s_gt, s_leq, s_geq]
  , s_plus, choice [s_mult, s_div]]

expression = foldr binOpExpr (maybeSubtract prodExpr) operations
  <?> "an expression"

prodExpr = do
    e1    <- elExpr
    sign' <- optionMaybe (s_mult <|> s_div)
    case sign' of
         Nothing -> return e1
         Just s  -> returny (BinOpExpr s e1) prodExpr

elExpr = do
      s_lpar
      e <- expression
      s_rpar
      return e
  <|> returny (UnOpExpr Scanner.Minus) negative
  <|> returny ElemPrim call
  <|> returny Str string
  <|> returny Num number
  <|> returny Bln boolean
  <|> table
  <?> "string, number, array initialization or expression in parentheses"

maybeSubtract ::    GenParser PosToken st (Expr Ident)
                 -> GenParser PosToken st (Expr Ident)
maybeSubtract expr = try (do
      e <- expr
      s_minus
      maybeSubtract (returny (BinOpExpr Scanner.Minus e)  prodExpr))
  <|> expr

negative = s_minus >> prodExpr

suffix :: GenParser PosToken st (Suffix Ident)
suffix = returny ArgSuff (parameters expression)
  <|> do
      s_lsqr
      key <- expression
      s_rsqr
      return $ KeySuff key
  <?> "function arguments, array key or nothing"

table = do
    s_lcurly
    els <- sepBy tabelem s_comma
    s_rcurly
    return $ Table els
  <?> "array initialization"
    where tabelem  = do
            key <- expression
            s_colon
            val <- expression
            return (key, val)

call :: GenParser PosToken st (Call Ident)
call = do
  id <- ident
  returny (Call id) (many suffix)

ifexpr :: GenParser PosToken st (Statement Ident)
ifexpr = do
      parseKeyword IfKw
      pred <- expression
      parseKeyword ThenKw
      cons <- statement
      returny (IfExpr pred cons) elseStmt
  <?> "if-expression"

elseStmt:: GenParser PosToken st (ElseStmt Ident)
elseStmt = do
      parseKeyword ElseKw
      returny Else statement
  <|> return EndIf

while :: GenParser PosToken st (Statement Ident)
while = do
    parseKeyword WhileKw
    cond <- expression
    parseKeyword DoKw
    returny (While cond) statement
  <?> "while loop"

callp :: GenParser PosToken st (Statement Ident)
callp = do
    parseKeyword CallKw
    cally <- call
    s_semic
    return $ CallP cally
  <?> "procedure call"

returnV :: GenParser PosToken st (Statement Ident)
returnV = do
    parseKeyword ReturnKw
    expr <- expression
    s_semic
    return $ Return expr
  <?> "return expression"

assign:: GenParser PosToken st (Statement Ident)
assign = do
    el  <- call
    s_assign
    val <- expression
    s_semic
    return $ Assign el val
  <?> "an assignment"
