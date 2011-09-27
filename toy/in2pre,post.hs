{-# LANGUAGE ScopedTypeVariables #-}

import Text.ParserCombinators.Parsec hiding ((<|>))
import Control.Applicative
import Control.Monad.Writer
import Data.List

data Tree = Equiv Tree Tree | Imply Tree Tree | Not Tree | And Tree Tree | Or Tree Tree | Var Char deriving (Show)

parser :: Int -> CharParser st Tree
parser d = if d < 4
           then foldl1 ([Equiv,Imply,Or,And]!!d) <$> go
           else (char '!' >> parser d >>= pure . Not) <|> (alphaNum >>= pure . Var) <|> (char '(' *> parser 0 <* char ')')
  where
    go = parser (d+1) >>= \x -> optionMaybe (string (["<->","->","|","&"]!!d) *> go) >>= \y -> pure $ maybe [x] (x:) y

postOrder :: Tree -> String
postOrder = execWriter . dfs
  where
    dfs (Var c) = tell [c]
    dfs (Not x) = dfs x >> tell "!"
    dfs (And x y) = dfs x >> dfs y >> tell "&"
    dfs (Or x y) = dfs x >> dfs y >> tell "|"
    dfs (Imply x y) = dfs x >> dfs y >> tell "->"
    dfs (Equiv x y) = dfs x >> dfs y >> tell "<->"

preOrder :: Tree -> String
preOrder = execWriter . dfs
  where
    dfs (Var c) = tell [c]
    dfs (Not x) = tell "!" >> dfs x
    dfs (And x y) = tell "&" >> dfs x >> dfs y
    dfs (Or x y) = tell "|" >> dfs x >> dfs y
    dfs (Imply x y) = tell "->" >> dfs x >> dfs y
    dfs (Equiv x y) = tell "<->" >> dfs x >> dfs y

main = getLine >>= \line -> case parse (parser 0) "" line of
  Left e -> putStrLn "Error parsing input:" >> print e
  Right r -> print r >> putStrLn (preOrder r) >> putStrLn (postOrder r)