module Minilang.Types (Expr(..), ArithExpr(..), Op(..), MiniProg, ErrorM, TCError(..), PrimType(..), Context) where
  import Control.Monad.Except
  import Data.Map (Map)
  
  data Op = Plus | Minus | Div | Mult deriving (Eq)

  data PrimType = Float | Int deriving (Eq)

  data ArithExpr = ICons Int 
    | FCons Float 
    | Neg ArithExpr
    | AOp Op ArithExpr ArithExpr 
    | ARef String deriving (Eq)

  data Expr = While ArithExpr [Expr]
    | If ArithExpr [Expr] [Expr]
    | Read String
    | Print ArithExpr 
    | Decl String PrimType 
    | Assign String ArithExpr 
    | Nop

  data TCError = ArithError
    | DeclError String
    | RefError 
    | TypeError String
    | ImpossibleError String
    deriving (Show, Eq)

  type Context = Map String PrimType

  type ErrorM = Either (TCError, Context)

  type MiniProg = [Expr]

  instance Show Op where
    show Plus = "+"
    show Minus = "-"
    show Mult = "*"
    show Div = "/"

  instance Show PrimType where
    show Float = "float"
    show Int = "int"

  instance Show Expr where 
    show (While a el) = "while " ++ show a ++ " do\n" ++ show el ++ "done\n"
    show (If a tl []) = "if " ++ show a ++ " then\n" ++ show tl ++ "endif\n"
    show (If a tl fl ) = "if " ++ show a ++ " then\n" ++ show tl ++ "else\n" ++ show fl ++ "endif\n"
    show (Read id) = "read " ++ id ++ ";\n"
    show (Print a) = "print " ++ show a ++ ";\n"
    show (Decl id tp) = "var " ++ id ++ " : " ++ show tp ++ ";\n"
    show (Assign id a) = id ++ " = " ++ show a ++ ";\n"
    showList el = (concat (map (show) el) ++)

  instance Show ArithExpr where
    show (ICons v) = show v
    show (FCons v) = show v
    show (Neg a)   = "-" ++ show a
    show (AOp o al ar) = (show al) ++ show o ++ (show ar)
    show (ARef id) = id