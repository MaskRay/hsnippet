module Eval (
  eval
  ) where

import Control.Monad
import Data.Maybe

import Parser

shift :: Int -> Term -> Term
shift inc term = helper f term
  where
    f c v@(Var i) 
      | i >= c = Var (i + inc)
      | otherwise = v

sub :: Int -> Term -> Term -> Term
sub idx replacemnt term = helper f term
  where
    f c v@(Var i)
      | c + idx == i = shift c replacemnt
      | otherwise = v

helper :: (Int -> Term -> Term) -> Term -> Term
helper valHandler term = go 0 term
  where
    go c v@(Var _) = valHandler c v
    go c (Abs x t) = Abs x (go (c+1) t)
    go c (App t1 t2) = App (go c t1) (go c t2)

isVal (Abs _ _) = True
isVal (Var _) = True
isVal _ = False

eval1 :: Term -> Maybe Term
eval1 (App (Abs _ t1) t2) | isVal t2 = Just $ shift (-1) (sub 0 (shift 1 t2) t1)
eval1 (App t1 t2) | not (isVal t2) = Just $ App t1 (fromJust $ eval1 t2)
eval1 (App t1 t2) | not (isVal t1) = Just $ App (fromJust $ eval1 t1) t2
eval1 _ = Nothing

eval :: Context -> Term -> (Term, Context)
eval ctx t = case t of
  Bind n t -> let t' = f t in (t', (n, t') : ctx)
  _ -> (f t, ctx)
  where
    f t = let t' = eval1 t in maybe t f t'
