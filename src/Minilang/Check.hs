module Minilang.Check (check, arithmetic, Context) where
  import Data.Map (Map, empty, (!), member, insert)
  import Minilang.Types
  import Control.Monad.Except
  import Control.Applicative ((<*))

  check :: MiniProg -> ErrorM (Context)
  check = expr empty

  statement :: Context -> Expr -> ErrorM Context
  statement ctxt (While cond body) = case arithmetic ctxt cond of
    Left a -> throwError a 
    Right tp -> if tp == Int then (return ctxt) <* (return $ expr ctxt body) else throwError $ (TypeError "Unexpected float", ctxt)

  statement ctxt (If cond l r) = case arithmetic ctxt cond of
    Left a ->  throwError a 
    Right tp -> if tp == Int then head (map (expr ctxt) [l,r] )else throwError $ (TypeError "Unexpected float", ctxt)

    --foldl (\p n -> case p of
      --Left _ -> p
      --Right t -> expr t n) (return ctxt) [l,r,[Nop]]

  statement ctxt (Decl var kind) = if not (member var ctxt) 
    then return $ insert var (kind) ctxt
    else throwError $ (DeclError $ "Var `"++ var ++"` already declared.", ctxt)

  statement ctxt (Assign var val) = case arithmetic ctxt val of
    Left a -> throwError a
    Right a -> if (member var ctxt) && ((ctxt ! var) == Float || (ctxt ! var) == a)
      then return $ ctxt
      else throwError (RefError, ctxt )

  statement ctxt _ = return $ ctxt

  expr :: Context -> MiniProg -> ErrorM Context
  expr c e = foldl (\p n -> case p of
    Left _ -> p
    Right t -> (statement t n)) (return c) e

  arithmetic :: (Context) -> ArithExpr -> ErrorM PrimType
  arithmetic ctxt (AOp _ l r) = case (arithmetic ctxt l, arithmetic ctxt r) of
    (Right a, Right b) -> if a == Float || b == Float then return Float else return Int
    (_, _) -> throwError (ArithError, ctxt)
  arithmetic ctxt (ARef s )   = if member s ctxt then return (ctxt ! s) else throwError (ArithError, ctxt)
  arithmetic ctxt (ICons _)   = return $ Int
  arithmetic ctxt (FCons _)   = return $ Float
  arithmetic ctxt (Neg a)     = arithmetic ctxt a
