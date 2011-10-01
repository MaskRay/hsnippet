import Control.Applicative
import Control.Monad.Writer
import Control.Monad.State
import Data.Char
import Data.List
import System.Environment
import Text.ParserCombinators.Parsec hiding ((<|>))

data Tree = Equiv Tree Tree | Imply Tree Tree | Not Tree | And Tree Tree | Or Tree Tree | Var Char deriving (Show)

parser :: Int -> CharParser st Tree
parser d = if d < 4
           then foldl1 ([Equiv,Imply,Or,And]!!d) <$> go
           else (char '!' >> parser d >>= pure . Not) <|> (alphaNum >>= pure . Var) <|> (char '(' *> parser 0 <* char ')')
  where
    go = parser (d+1) >>= \x -> optionMaybe (string (["<->","->","|","&"]!!d) *> go) >>= \y -> pure $ maybe [x] (x:) y

traverse :: Bool -> (String -> StateT Bool (Writer String) ()) -> Tree -> String
traverse p f = execWriter . flip runStateT False . dfs
  where
    dfs (Var c) = when p (f [c]) >> when (not p) (f [c])
    dfs (Not x) = when p (f "!") >> dfs x >> when (not p) (f "!")
    dfs (And x y) = when p (f "&") >> dfs x >> dfs y >> when (not p) (f "&")
    dfs (Or x y) = when p (f "|") >> dfs x >> dfs y >> when (not p) (f "|")
    dfs (Imply x y) = when p (f "->") >> dfs x >> dfs y >> when (not p) (f "->")
    dfs (Equiv x y) = when p (f "<->") >> dfs x >> dfs y >> when (not p) (f "<->")

main = do
  args <- getArgs
  let isTeX = any (`elem` ["-t", "--tex"]) args
  let mapping = if isTeX then \c -> do
        last <- get
        when last (tell "\\; ")
        case c of
          "!" -> tell "\\neg "
          "&" -> tell "\\wedge "
          "|" -> tell "\\vee "
          "->" -> tell "\\rightarrow "
          "<->" -> tell "\\leftrightarrow "
          x -> tell x
        put True
                else \x -> get >>= \last -> when last (tell " ") >> tell x >> put True
  when isTeX $ putStrLn "\\documentclass[12pt]{article}\n\\parindent 0em\n\\pagestyle{empty}\n\\begin{document}"
  getLine >>= \line -> case parse (parser 0 <* eof) "" (filter (not . isSpace) line) of
    Left e -> putStrLn "Error parsing input:" >> print e
    Right r -> when isTeX (putStr "$$") >> putStr (traverse True mapping r) >> (if isTeX then (putStr "$$\n") else putStrLn "") >> when isTeX (putStr "$$") >> putStr (traverse False mapping r) >> (if isTeX then (putStr "$$\n") else putStrLn "")
  when isTeX $ putStrLn "\\end{document}"
