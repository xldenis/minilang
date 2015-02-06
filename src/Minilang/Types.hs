module Minilang.Types (Expr(..), ArithExpr(..), Ops(..), MiniProg, ErrorM, TCError(..), PrimType(..)) where
  import Control.Monad.Except

  data Ops = Plus | Minus | Div | Mult deriving (Eq)

  data PrimType = Float | Int deriving (Eq)

  data ArithExpr = ICons Int 
    | FCons Float 
    | Neg ArithExpr
    | AOp Ops ArithExpr ArithExpr 
    | ARef String deriving (Eq)

  data Expr = While ArithExpr [Expr]
    | If ArithExpr [Expr] [Expr]
    | Read String
    | Print ArithExpr 
    | Decl String String 
    | Assign String ArithExpr 
    | Nop

  data TCError = ArithError
    | DeclError String
    | RefError 
    | ImpossibleError String
    deriving (Show, Eq)

  type ErrorM = Either TCError

  type MiniProg = [Expr]

  instance Show Ops where
    show Plus = "+"
    show Minus = "-"
    show Mult = "*"
    show Div = "/"

  instance Show Expr where 
    show (While a el) = "while " ++ show a ++ " do\n" ++ show el ++ "done\n"
    show (If a tl []) = "if " ++ show a ++ " then\n" ++ show tl ++ "endif\n"
    show (If a tl fl ) = "if " ++ show a ++ " then\n" ++ show tl ++ "else\n" ++ show fl ++ "endif\n"
    show (Read id) = "read " ++ id ++ ";\n"
    show (Print a) = "print " ++ show a ++ ";\n"
    show (Decl id tp) = "var " ++ id ++ " : " ++ tp ++ ";\n"
    show (Assign id a) = id ++ " = " ++ show a ++ ";\n"
    showList el = (concat (map (show) el) ++)

  instance Show ArithExpr where
    show (ICons v) = show v
    show (FCons v) = show v
    show (Neg a)   = "-" ++ show a
    show (AOp o al ar) = (show al) ++ show o ++ (show ar)
    show (ARef id) = id