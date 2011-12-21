{-# LANGUAGE FlexibleContexts #-}

module Scanner (
  -- * Data type for signs (parentheses, arithmetic operators etc.)
  GSign(..)
  -- * Parses input string into lexemes
  , gLex
  -- * Data type for non-keyword-filtered tokens: identifiers, numbers,
  --   strings and signs
  , Token(..)
  -- * Pair of token and its source position
  , PosToken
  ) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim hiding (try)
import Control.Monad.Identity

data GSign =   LPar | RPar | LSqr | RSqr | LCurly | RCurly
             | Comma | Semic | Colon | Assign
             | And | Or | Equals | NotEq | Lt | Gt | LEq | GEq
             | Plus | Minus | Mult | Div
  deriving (Show, Eq)

data Token =   Ident String
             | Num Double
             | Str String
             | Sign GSign
  deriving Show

type PosToken = (SourcePos, Token)

signs :: [(String, GSign)]
signs = [  ("(", LPar), (")", RPar), ("[", LSqr), ("]", RSqr), ("{", LCurly)
         , ("}", RCurly), (",", Comma), (";", Semic), (":", Colon)
         , ("&&", And), ("||", Or), ("==", Equals), ("/=", NotEq), ("<=", LEq)
         , (">=", GEq), (">", Gt), ("<", Lt), ("=", Assign), ("+", Plus)
         , ("-", Minus), ("*", Mult), ("/", Div)]

gLex :: FilePath -> String -> Either ParseError [PosToken]
gLex = parse lexString

lexString :: GenParser Char st [PosToken]
lexString = do
  spaces
  ts <- sepEndBy (choice [str, num, sign, ident]) spaces
  eof
  return ts

str = do
    p <- getPosition
    char '"'
    s <- many $ noneOf "\""
    char '"'
    return (p, Str s)
  <?> "a string"

num = do
    let digits    = many1 digit
    p           <- getPosition
    let returny n = return (p, Num $ read n)
    digs1       <- digits
    try (char '.' >> (returny . (digs1 ++) . ("." ++) =<< digits))
      <|> returny digs1
  <?> "a number"

readReturn ::    String
              -> Token
              -> ParsecT String st Identity PosToken
readReturn str f = do
    p <- getPosition
    try (string str)
    return (p, f)
  <?> "the " ++ str ++ " keyword"

sign = choice $ map (\(s, t) -> readReturn s (Sign t)) signs

identifier = do
  firstLet <- letter
  return . (firstLet:) =<< many alphaNum

ident :: GenParser Char st PosToken
ident = do
  p <- getPosition
  return . (,) p . Ident =<< identifier
