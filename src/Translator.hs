module Translator (
  -- * Translates parsed grammar tree into Java
  translate
  ) where

import Grammar
import Link
import Scanner (GSign(..))

import System.IO
import Data.Maybe (fromMaybe)
import Control.Monad.State
import System.FilePath.Posix (dropExtension)

data IndentState = IndentState {  indent :: Int
                                , string :: String}

translate ::   String 
            -> Either String (ProgTree LinkId)
            -> Either String (IO ())
translate sourceFileName = fmap (translateToJava sourceFileName)

translateToJava :: String -> ProgTree LinkId -> IO ()
translateToJava name tree = do
  openFile name ReadMode
  let className          = dropExtension name
      IndentState _ code = execState (jCreateClass className tree) $
        IndentState 0 ""
  writeFile (className ++ ".java") code

standardFunctions = [("print", \args -> case args of
  [arg] -> do
    startLine "System.out.println("
    jCall (Call arg [])
    endLine ");"
  _     -> fail "Function 'print' must have one argument")]

jCreateClass :: String -> ProgTree LinkId -> State IndentState ()
jCreateClass className tree = do
  mapM jDecl tree
  state <- get
  let IndentState _ funcVals = state
  put $ state {string = "", indent = 0}
  writeLines
   [  "import runtimy.*;\n"
    , "public class " ++ className ++ " {"]
  addIndent
  writeLine "public static void main(String[] args) {"
  addIndent
  writeLines $ lines funcVals
  writeLine "main.get().apply();"
  delIndent
  writeLine "}\n"
  delIndent
  writeLine "}"

continueString :: String -> State IndentState ()
continueString str = do
  state <- get
  put $ state {string = string state ++ str}

startLine :: String -> State IndentState ()
startLine str = do
  state <- get
  put $ state {string = concat
        [string state, replicate (indent state) '\t', str]}

endLine    = continueString . (++"\n")
writeLine  = startLine . (++"\n")
writeLines = mapM writeLine
newLine    = startLine ""

addIndent :: State IndentState ()
addIndent = changeIndent (+)

delIndent :: State IndentState ()
delIndent = changeIndent (-)

changeIndent :: (Int -> Int -> Int) -> State IndentState ()
changeIndent f = do
  state <- get
  put $ state {indent = indent state `f` 1}

jDecl :: Decl LinkId -> State IndentState ()
jDecl (VarDecl lnk maybeVal) = do
  startLine $ "final Variable " ++ (varName lnk) ++ " = new Variable("
  maybe
    (return ())
    (\expr -> jExpression expr)
    maybeVal
  endLine ");"
jDecl (Decl lnk args body isFunction) = do
  let fName = varName lnk
  writeLine $ "final Variable " ++ fName ++ " = new Variable();"
  writeLine $ fName ++ ".set(new FunctionVal(new Call(\"" ++ fName ++ "\") {"
  addIndent
  writeLine $ "public Value apply(Value... variables) {"
  addIndent
  writeLine $ "checkParams(" ++ show (length args) ++ ", variables);"
  writeLines declareArguments
  fromMaybe
    (jStatement body)
    (do
      maybeFunction <- lookup (getId lnk) standardFunctions
      return $ maybeFunction args)
  writeLine $ if isFunction then "" else "return null;"
  delIndent
  writeLine "}"
  delIndent
  writeLine "}));\n"
    where declareArguments = declareArgumenties args $ 0
          declareArgumenties [] _     = []
          declareArgumenties (x:xs) n = ("final Variable " ++ varName x
            ++ " = new Variable(variables[" ++ show n ++ "]);")
            : declareArgumenties xs (n + 1)
          getId = snd . fst

jStatement (Block stmts)                             = do
  writeLine "{"
  addIndent
  mapM jStatement stmts
  delIndent
  writeLine "}"
jStatement (IfExpr pred conseq alterns)              = do
  startLine "if ("
  jExpression pred
  endLine ".isTrue()) {"
  addIndent
  jStatement conseq
  delIndent
  startLine "}"
  jElse alterns
jStatement (While condition body)                    = do
  startLine "while ("
  jExpression condition
  endLine ".isTrue()) {"
  addIndent
  jStatement body
  delIndent
  writeLine "}"
jStatement (CallP call)                              = do
  newLine
  jCall call
  endLine ";"
jStatement (Return expr)                             = do
  startLine "return "
  jExpression expr
  endLine ";"
jStatement (DeclSt decl)                             = jDecl decl
jStatement (Grammar.Assign (Call lnk suffixes) expr) = do
  let expr' = jExpression expr
  if null suffixes
    then do
          startLine $ varName lnk ++ ".set("
          expr'
          endLine ");"
    else do
         newLine
         jCall (Call lnk (init suffixes))
         continueString ".putValue("
         let (KeySuff key) = last suffixes
         jExpression key
         continueString ", "
         expr'
         endLine ");"

varName :: LinkId -> String
varName ((_, id), addr) = id ++ (if id == "main" then "" else show addr)

jCall :: Call LinkId -> State IndentState ()
jCall (Call lnk [])    = continueString $ varName lnk ++ ".get()"
jCall (Call lnk suffs) = do
  continueString $ varName lnk
  if null suffs
    then continueString ".get()"
    else jCally suffs
      where writeArgs []         = return ()
            writeArgs (arg:args) = do
              jExpression arg
              if null args
                then return ()
                else do
                  continueString ", "
                  writeArgs args
            jCally suffs = case suffs of
              []                     -> return ()
              (ArgSuff args : _) -> do
                continueString ".get().apply("
                writeArgs args
                continueString ")"
              (KeySuff key : _)  -> do
                continueString ".get().getValue("
                jExpression key
                continueString ")"

jElse EndIf       = newLine
jElse (Else stmt) = do
  endLine " else {"
  addIndent
  jStatement stmt
  delIndent
  writeLine "}"

operations = [  (And, "and"), (Or, "or"), (Equals, "equal")
              , (NotEq, "notEqual"), (LEq, "lessOrEq"), (GEq, "greaterOrEq")
              , (Lt, "less"), (Gt, "greater"), (Plus, "add")
              , (Minus, "subtract"), (Mult, "multiply"), (Div, "divide")]

jExpression :: Expr LinkId -> State IndentState ()
jExpression (Num num)                        = continueString $
  "new DoubleVal(" ++ show num ++ ")"
jExpression (Bln bln)                        = continueString $
  "new BooleanVal(" ++ (if bln then "true" else "false") ++ ")"
jExpression (Str str)                        = continueString $
  "new StringVal(\"" ++ str ++ "\")"
jExpression (ElemPrim call)                  = jCall call
jExpression (BinOpExpr operator expr1 expr2) = do
  jExpression expr1
  continueString "."
  continueString $ fromMaybe "" $ lookup operator operations
  continueString "("
  jExpression expr2
  continueString ")"
jExpression (UnOpExpr Minus expr)            = do
  continueString "new DoubleVal(-1.0).multiply("
  jExpression expr
  continueString ")"
jExpression (UnOpExpr _ _)                   = fail
  "Unary operation can be only minus"
jExpression (Table table)                    =
  let (keys, values) = unzip table
  in do
    endLine "new ObjectVal()"
    addIndent
    sequence (zipWith (\key value -> do
        startLine ".field("
        jExpression key
        continueString ", "
        jExpression value
        endLine ")")
      keys values)
    delIndent
    newLine
