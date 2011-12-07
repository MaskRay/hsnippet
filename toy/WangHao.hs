import Control.Applicative
import Data.List
import Data.IORef
import Control.Monad.Reader
import Control.Monad.State
import Text.ParserCombinators.Parsec hiding ((<|>))
import qualified Text.PrettyPrint as PP

data Tree = Equiv Tree Tree | Imply Tree Tree | Not Tree | And Tree Tree | Or Tree Tree | Var Char deriving (Ord, Eq)

parser :: Int -> CharParser st Tree
parser d = between spaces spaces (if d < 4
            then chainr1 (parser (d+1)) $ ([Equiv,Imply,Or,And]!!d) <$ choice (map string ([["=","<->"],[">","->"],["+","|"],["*","&",""]]!!d))
            else Not <$> (oneOf "~!" *> parser d) <|> Var <$> alphaNum <|> between (char '(') (char ')') (parser 0)
           )

instance Show Tree where
  showsPrec _ (Var c) = showChar c
  showsPrec d (Not x) = showParen (d > 4) $ showChar '~' . showsPrec 4 x
  showsPrec d (And x y) = showParen (d > 3) $ showsPrec 3 x . showsPrec 3 y
  showsPrec d (Or x y) = showParen (d > 2) $ showsPrec 2 x . showChar '+' . showsPrec 2 y
  showsPrec d (Imply x y) = showParen (d > 1) $ showsPrec 1 x . showChar '>' . showsPrec 1 y
  showsPrec d (Equiv x y) = showParen (d > 0) $ showsPrec 0 x . showChar '=' . showsPrec 0 y
  showList [] s = s
  showList xs s = concat (intersperse "," (map show xs)) ++ s

data S = S { num :: IORef Int, l :: [Tree], ll :: [Tree], r :: [Tree], rr :: [Tree] }

wh :: StateT [Int] (ReaderT S IO) Bool
wh = do
  S{num=ref, l=x, ll=xx, r=y, rr=yy} <- ask
  prevNum <- lift . lift $ readIORef ref
  let
    output rule = lift $ do
      S{l=x, ll=xx, r=y, rr=yy} <- ask
      lift $ modifyIORef ref succ
      curNum <- lift $ readIORef ref
      liftIO . putStrLn . PP.render $ PP.text ("(" ++ show curNum ++ ") " ++ show (xx++x) ++ " =s=> " ++ show (yy++y)) PP.$$
        PP.nest 30 (PP.text ("(" ++ show prevNum ++ ") " ++ rule))
  case x of
    Var c:x' -> local (\s->s{l=x', ll=Var c:xx}) wh
    Not a:x' -> local (\s->s{l=x', r=a:y}) (output "~ =>" >> wh)
    And a b:x' -> local (\s->s{l=a:b:x'}) (output "* =>" >> wh)
    Or a b:x' -> (&&) <$> local (\s->s{l=a:x'}) (output "+ =>" >> wh) <*> local (\s->s{l=b:x'}) (output "+ =>" >> wh)
    Imply a b:x' -> (&&) <$> local (\s->s{l=b:x'}) (output "> =>" >> wh) <*> local (\s->s{l=x', r=a:y}) (output "> =>" >> wh)
    Equiv a b:x' -> (&&) <$> local (\s->s{l=a:b:x'}) (output "= =>" >> wh) <*> local (\s->s{l=x', r=a:b:y}) (output "= =>" >> wh)

    _ -> case y of
      Var c:y' -> local (\s->s{r=y', rr=Var c:yy}) wh
      Not a:y' -> local (\s->s{l=a:x, r=y'}) (output "=> ~" >> wh)
      And a b:y' -> (&&) <$> local (\s->s{r=a:y'}) (output "=> *" >> wh) <*> local (\s->s{r=b:y'}) (output "=> *" >> wh)
      Or a b:y' -> local (\s->s{r=a:b:y'}) (output "=> +" >> wh)
      Imply a b:y' -> local (\s->s{l=a:x, r=b:y'}) (output "=> >" >> wh)
      Equiv a b:y' -> (&&) <$> local (\s->s{l=a:x, r=b:y'}) (output "= =>" >> wh) <*> local (\s->s{l=b:x, r=a:y'}) (output "=> =" >> wh)

      _ -> modify (prevNum:) >> return (intersect xx yy /= [])

main = do
  line <- getLine
  case parse (parser 0 <* eof) "" line of
    Left e -> print e
    Right t -> do
      ref <- newIORef 0
      putStrLn $ "(0)  =s=> " ++ show t
      (r, ls) <- runReaderT (runStateT wh []) (S {num=ref, l=[], ll=[], r=[t], rr=[]})
      putStrLn $ "formulas: " ++ show ls
      putStrLn $ if r then "theorem" else "not theorem"
