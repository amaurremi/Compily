-- | Some auxilary functions
module AuxFuncs where

import Filter (Token(..), PosToken, DKeyword(..))
import qualified Scanner (GSign(..))
import Grammar

import Text.ParserCombinators.Parsec hiding (string)

ftoken test = token (show . snd) fst (test . snd)

parseToken tokenF val = ftoken (\t ->
  if t == tokenF val then Just val else Nothing)

ident :: GenParser PosToken st Ident
ident = do
  pos <- getPosition
  ftoken (\t -> case t of
    Ident i -> Just (pos, i)
    _       -> Nothing)

string :: GenParser PosToken st String
string = ftoken (\t -> case t of
  StrT s -> Just s
  _      -> Nothing)

number :: GenParser PosToken st Double
number = ftoken (\t -> case t of
  NumT n -> Just n
  _      -> Nothing)

boolean :: GenParser PosToken st Bool
boolean = do
  bool <- choice $ map parseKeyword [TrueKw, FalseKw]
  return $ bool == TrueKw

parseKeyword :: DKeyword -> GenParser PosToken st DKeyword
parseKeyword = parseToken Keyword

sign :: Operator -> GenParser PosToken st Operator
sign = parseToken Sign

binOpExpr operator priorFunction = try (do
      e1 <- priorFunction
      o  <- operator
      returny (BinOpExpr o e1) (binOpExpr operator priorFunction))
  <|> priorFunction

returny t p = return . t =<< p

todo = try pzero

s_lpar     = sign Scanner.LPar          -- (
s_rpar     = sign Scanner.RPar          -- )
s_comma    = sign Scanner.Comma         -- ,
s_lsqr     = sign Scanner.LSqr          -- [
s_rsqr     = sign Scanner.RSqr          -- ]
s_lcurly   = sign Scanner.LCurly        -- {
s_rcurly   = sign Scanner.RCurly        -- }
s_semic    = sign Scanner.Semic         -- ;
s_colon    = sign Scanner.Colon         -- :
s_assign   = sign Scanner.Assign        -- =
s_and      = sign Scanner.And           -- &&
s_or       = sign Scanner.Or            -- ||
s_equals   = try $ sign Scanner.Equals  -- ==
s_noteq    = sign Scanner.NotEq         -- /=
s_lt       = sign Scanner.Lt            -- <
s_gt       = sign Scanner.Gt            -- >
s_leq      = try $ sign Scanner.LEq     -- <=
s_geq      = try $ sign Scanner.GEq     -- >=
s_plus     = sign Scanner.Plus          -- +
s_minus    = sign Scanner.Minus         -- -
s_mult     = sign Scanner.Mult          -- *
s_div      = sign Scanner.Div           -- /
