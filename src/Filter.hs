module Filter (
  -- * Filters keywords out of lexemes parsed with gLex ('Scanner' module)
  filterKeywords
  -- * Abstract data type for filtered DL1 tokens:
  -- identifiers, keywords, numbers, strings and signs
  , Token(..)
  -- * Pair of tokens matched with their source positions (SourcePos)
  , PosToken
  -- * Container of DL1's keywords
  , DKeyword(..)
  ) where

import qualified Scanner
import Text.ParserCombinators.Parsec (ParseError, SourcePos)

data DKeyword =   FuncKw | ProcKw | VarKw | IfKw | ThenKw | ElseKw
                | WhileKw | DoKw | CallKw | ReturnKw | TrueKw | FalseKw
  deriving (Show, Eq)

data Token =   Ident String
             | Keyword DKeyword
             | NumT Double
             | StrT String
             | Sign Scanner.GSign
  deriving (Show, Eq)

type PosToken = (SourcePos, Token)

filterKeywords ::    Either ParseError [Scanner.PosToken]
                  -> Either ParseError [PosToken]
filterKeywords = fmap filteredTokens

filteredTokens :: [Scanner.PosToken] -> [PosToken]
filteredTokens = map exposeKeyword

keywords = [  ("function", FuncKw), ("procedure", ProcKw), ("var", VarKw)
            , ("if", IfKw), ("else", ElseKw), ("then", ThenKw)
            , ("while", WhileKw), ("do", DoKw), ("call", CallKw)
            , ("return", ReturnKw), ("true", TrueKw), ("false", FalseKw)]

exposeKeyword :: Scanner.PosToken -> PosToken
exposeKeyword (pos, token) = (pos, case token of
  Scanner.Ident id -> let reduced = filter ((== id) . fst) keywords
    in if null reduced
      then Ident id
      else Keyword $ snd $ head reduced
  Scanner.Num n    -> NumT n
  Scanner.Str str  -> StrT str
  Scanner.Sign s   -> Sign s)
