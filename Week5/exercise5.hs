{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import qualified StackVM as S
import qualified Parser as P

data Token = Const Integer
           | Plus Token Token
           | Star Token Token
  deriving Show

class Expr expression where
    lit :: Integer -> expression
    mul :: expression -> expression -> expression
    add :: expression -> expression -> expression

-- Exercise 5
instance Expr S.Program where
    lit i = [S.PushI i]
    add e1 e2 = e1 ++ e2 ++ [S.Add]
    mul e1 e2 = e1 ++ e2 ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = eval . P.parseExp Const Plus Star

eval :: Maybe Token -> Maybe S.Program
eval Nothing = Nothing
eval (Just e) = Just (eval' e)

eval' :: Token -> S.Program
eval' (Const i) = lit i :: S.Program
eval' (Plus e1 e2) = add (eval' e1) (eval' e2) :: S.Program
eval' (Star e1 e2) = mul (eval' e1) (eval' e2) :: S.Program
