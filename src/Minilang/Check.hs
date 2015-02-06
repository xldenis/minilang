module Minilang.Check (check) where
  import Data.Map (Map, empty, (!), member, insert)
  import Minilang.Types
  import Control.Monad.Except
  import Control.Applicative ((<*))

  check :: MiniProg -> (Map String PrimType)
  check = expr empty

  statement :: (Map String PrimType) -> Expr -> ErrorM (Map String PrimType)
  statement ctxt (While cond body) = case arithmetic ctxt cond of
    Left a -> throwError a 
    Right _ -> (return ctxt) <* (return $ expr ctxt body)

  statement ctxt (If cond l r) = case arithmetic ctxt cond of
    Left a ->  throwError a 
    Right _ -> return $ foldl (\p n -> (expr ctxt n)) ctxt [l,r,[Nop]] -- wrong

  statement ctxt (Decl var kind) = if not (member var ctxt) 
    then case kind of
      "int" -> return $ insert var (Int) ctxt
      "float" -> return $ insert var (Float) ctxt
      _ -> throwError $ ImpossibleError "Invalid num type got past parser."
    else throwError $ DeclError $ "Var `"++ var ++"` already declared."

  statement ctxt (Assign var val) = case arithmetic ctxt val of
    Left a -> throwError a
    Right a -> if (member var ctxt) && a == (ctxt ! var)
      then return $ ctxt
      else throwError RefError 

  statement ctxt _ = return $ ctxt

  expr :: (Map String PrimType) -> MiniProg -> (Map String PrimType)
  expr c e = foldl (\p n -> case (statement p n) of
    Left a -> error $ show a
    Right a -> a ) c e

  arithmetic :: (Map String PrimType) -> ArithExpr -> ErrorM PrimType
  arithmetic ctxt (AOp _ l r) = case (arithmetic ctxt l, arithmetic ctxt r) of
    (Right a, Right b) -> if a == Float || b == Float then return Float else return Int
    (_, _) -> throwError ArithError
  arithmetic ctxt (ARef s )   = if member s ctxt then return (ctxt ! s) else throwError ArithError
  arithmetic ctxt (ICons _)   = return $ Int
  arithmetic ctxt (FCons _)   = return $ Float
