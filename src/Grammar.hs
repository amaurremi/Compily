-- | Data types representing DL1 syntax tree
module Grammar where

import Scanner (GSign)
import Text.ParserCombinators.Parsec (SourcePos)

type Operator    = GSign

-- | The upper level of the syntax tree is a list of function
--   or procedure declarations.
type ProgTree a  = [Decl a]

-- | A declaration may be a function or procedure declaration, or a
--   variable declaration.
data Decl a
  -- | A variable declaration contains a variable identifier and may contain
  --   an expression bound to it
    = VarDecl a (Maybe (Expr a))
  -- | A function or procedure declaration contains:
  --   (1) the name of the function or procedure
  --   (2) a list of the parameters it accepts
  --   (3) its body being represented in the 'Statement' data type
  --   (4) information whether it's a function (True) or procedure (False)
    | Decl a [a] (Statement a) Bool deriving Show

-- | An identifier is a pair of the identifier name and source position.
type Ident       = (SourcePos, String)

-- | The 'Key' type represents the key of associated arrays.
type Key         = Expr

-- | The abstract data type for expressions.
data Expr a
  -- | A binary operation. Stores the binary operator and the two operands
    = BinOpExpr Operator (Expr a) (Expr a)
  -- | A unary operation. Stores the unary operator and the operand
    | UnOpExpr Operator (Expr a)
  -- | A string primitive type
    | Str String
  -- | A number primitive type
    | Num Double
  -- | A boolean primitive type
    | Bln Bool
  -- | An associated array
    | Table [(Expr a, Expr a)]
  -- | The call of either a function, or of an array element
    | ElemPrim (Call a) deriving Show

-- | The suffix is used for function or procedure calls (ArgSuff),
--   where it stores the arguments of the called function, or for array calls
--   (KeySuff), where it stores the key for the requested value.
data Suffix  a   =   ArgSuff [Expr a]
                   | KeySuff (Key a) deriving Show

-- | A variable, function or array element call.
data Call a      =   Call a [Suffix a] deriving Show

-- A statement or the body of a function or procedure.
data Statement a
  -- | Conditional statement; storing predicate expression, and consequent
  --   and alternative statements
    = IfExpr (Expr a) (Statement a) (ElseStmt a)
  -- | While loop; storing predicate expression and loop body statement
    | While (Expr a) (Statement a)
  -- | Procedure call
    | CallP (Call a)
  -- | Function return statement
    | Return (Expr a)
  -- | Block of statements
    | Block [Statement a]
  -- | Variable, function or procedure declaration (for closures)
    | DeclSt (Decl a)
  -- Assignments; storing a variable call and the expression to be assigned
  -- to that variable
    | Assign (Call a) (Expr a) deriving Show

-- | Alternative statement used in conditional
data ElseStmt a  =   Else (Statement a)
                   | EndIf deriving Show

