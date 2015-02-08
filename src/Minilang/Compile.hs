module Minilang.Compile (compile) where
  import qualified Data.Map as M
  import Minilang.Types
  import Control.Monad.Except
  import Minilang.Check(arithmetic, Context)

  compile :: Context -> MiniProg -> String

  compile ctxt prog = headers ++ "int main(void)" ++ scope ctxt prog

  compile' ctxt prog = concat $ map (compileStatement ctxt) prog

  compileStatement :: Context -> Expr -> String
  compileStatement ctxt stmt = case stmt of
    While acnd body -> "while("++(compileArithmetic acnd)++")"++ scope ctxt body
    
    If acnd left [] -> "if("++compileArithmetic acnd ++ ")" ++ scope ctxt left
    If acnd left right -> "if("++compileArithmetic acnd ++ ")" ++ scope ctxt left ++ "else"++ scope ctxt right
    
    Read ref -> case M.lookup ref ctxt of
      Just Float -> "scanf(\"%f\",&"++ref++");\n"
      Just Int ->"scanf(\"%d\",&"++ref++");\n"
      Nothing -> "error"
    
    Print expr -> case arithmetic ctxt expr of
      Right Float -> "printf(\"%f\","++compileArithmetic expr++");\n"
      Right Int ->"printf(\"%d\","++compileArithmetic expr++");\n"
      Left _ -> "error"
    
    Decl var tp -> (show tp)++ " "++var++";\n"
    
    Assign var val -> var ++" = "++ compileArithmetic val ++ ";\n" 

  compileArithmetic :: ArithExpr -> String
  compileArithmetic stmt = show stmt
    
  scope ctxt body = "{\n" ++ compile' ctxt body ++ "}\n"

  headers :: String
  headers = "#import <stdio.h>\n#import <stdlib.h>\n"