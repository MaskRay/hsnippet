module Main where

import Data.Char
import qualified Control.Exception as E
import Text.ParserCombinators.Parsec
import System.Exit
  
import Parser
import Eval

parseUntyped ctx str = runParser statement ctx "" str

main = forever []
  where
    strip [] = []
    strip ('#':_) = []
    strip (x:xs) = x : strip xs
    forever ctx = do
      s <- fmap strip $ E.catch getLine (\(E.SomeException _) -> exitSuccess)
      if all (isSpace) s
        then forever ctx
        else case parseUntyped ctx s of
        Left err -> putStrLn (show err) >> forever ctx
        Right v -> let (v', ctx') = eval ctx v
                   in putStrLn (ppTerm ctx v') >> forever ctx'
