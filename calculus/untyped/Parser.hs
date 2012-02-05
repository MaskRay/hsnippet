module Parser (
  Term(..), Context, Name, ppTerm, statement
  ) where

import Control.Applicative hiding (many, (<|>))
import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec
import System.IO.Unsafe

type Name = String
type Context = [(Name, Term)]
data Term = Dummy Name
          | Var Int
          | Abs Name Term
          | App Term Term
          | Bind Name Term
            deriving (Show, Eq)

identifier = spaces >> ((:) <$> letter) <*> many (try alphaNum <|> oneOf "_?'")

bind :: GenParser Char Context Term
bind = do
  n <- identifier
  spaces
  char '='
  spaces
  ctx <- getState
  let emptyDefine = (n, Dummy n) <$ eof
      define = term >>= \t -> return (n, t)
  item <- try emptyDefine <|> define
  updateState $ (item:)
  return $ Bind n (snd item)

getIndex ctx name = findIndex ((==name) . fst) ctx

var :: GenParser Char Context Term
var = do
  spaces
  n <- identifier
  ctx <- getState
  case getIndex ctx n of
    Nothing -> fail $ "unbound variable: " ++ n
    Just i -> case snd (ctx!!i) of
      Dummy _ -> return $ Var i
      v -> return v

abstract :: GenParser Char Context Term
abstract = do
  spaces
  string "\\" <|> string "lambda"
  n <- identifier
  spaces
  char '.'
  updateState $ ((n, Dummy n):)
  t <- term
  updateState tail
  return $ Abs n t

term = chainl1 (try (spaces >> between (char '(') (char ')') term) <|> try abstract <|> var <?> "identifier") (pure App)

statement = try bind <|> term

ppTerm ctx (Bind n t) = n ++ " = " ++ ppTerm ctx t
ppTerm ctx (App t1 t2) = ppTerm ctx t1 ++ " " ++ ppTerm ctx t2
ppTerm ctx (Abs n t) = let (n', ctx') = pickFreshName ctx n
                       in "(\\" ++ n' ++ ". " ++ ppTerm ctx' t ++ ")"
ppTerm ctx (Var i) = fst (ctx!!i)
ppTerm ctx (Dummy n) = n

pickFreshName :: Context -> Name -> (Name, Context)
pickFreshName ctx n =
  let n' = go ctx n
  in (n', (n', Dummy n') : ctx)
  where
    go [] n = n
    go (c:cs) n = if n == fst c then go cs (n ++ "'")
                  else go cs n