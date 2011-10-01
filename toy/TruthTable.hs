import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List
import Data.Maybe
import Foreign.Marshal.Utils (fromBool)
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec hiding ((<|>))
import Text.Printf

type Table = [(Char, Bool)]
  
data Tree = Equiv Tree Tree | Imply Tree Tree | Not Tree | And Tree Tree | Or Tree Tree | Var Char deriving (Show)

parser :: Int -> CharParser st Tree
parser d = if d < 4
           then foldl1 ([Equiv,Imply,Or,And]!!d) <$> go
           else (char '!' >> parser d >>= pure . Not) <|> (alphaNum >>= pure . Var) <|> (char '(' *> parser 0 <* char ')')
  where
    go = parser (d+1) >>= \x -> optionMaybe (string (["<->","->","|","&"]!!d) *> go) >>= \y -> pure $ maybe [x] (x:) y

getVars :: Tree -> Writer [Char] ()
getVars (Var c) = tell [c]
getVars (Not x) = getVars x
getVars (And x y) = getVars x >> getVars y
getVars (Or x y) = getVars x >> getVars y
getVars (Imply x y) = getVars x >> getVars y
getVars (Equiv x y) = getVars x >> getVars y

eval :: Tree -> Reader Table Bool
eval (Var c) = fromJust <$> asks (lookup c)
eval (Not x) = not <$> eval x
eval (And x y) = (&&) <$> eval x <*> eval y
eval (Or x y) = (||) <$> eval x <*> eval y
eval (Imply x y) = ((||) . not) <$> eval x <*> eval y
eval (Equiv x y) = (==) <$> eval x <*> eval y

main = do
  args <- getArgs
  let isTeX = length args > 0 && (head args == "-t" || head args == "--tex")
  let calate = intercalate $ if isTeX then " & " else " "
  getLine >>= \line -> case parse (parser 0) "" line of
    Left e -> print e
    Right t -> do
      let var = nub $ sort $ execWriter (getVars t)
          truth = map (\vs -> runReader (eval t) (zip var vs)) (mapM (const [False,True]) [1..length var])
          disjunctive = map snd $ filter fst $ zip truth [0..]
          conjunctive = reverse $ map (((2^length var-1)-) . snd) $ filter (not . fst) $ zip truth [0..]

      if isTeX
        then do
        printf "\\documentclass[12pt]{article}\n\\parindent 0em\n\\pagestyle{empty}\n\\begin{document}\n\\begin{tabular}{%s}\n" $ replicate (length var+1) 'l'
        putStrLn $ (++"\\\\\n\\hline") . intercalate " & " $ map (("\\textit{"++) . (++"}")) $ map pure var ++ ["result"]
        else putStrLn $ intercalate " " $ map pure var ++ [line]

      forM_ (zip truth $ mapM (const [False,True]) [1..length var]) $ \(b,vs) -> do
        putStr $ calate $ map (show . fromBool) $ vs ++ [b]
        if isTeX
          then putStrLn "\\\\"
          else putStrLn ""

      when isTeX $ putStrLn "\\end{tabular}\n"
      putStr "propositional variables: "
      if isTeX
        then putStrLn $ ('$':).(++"$") $ intercalate "\\;" $ map pure var
        else putStrLn $ intersperse ' ' var

      when isTeX $ putStrLn ""
      putStr "principal disjunctive normal form: "
      if null disjunctive
        then putStrLn "empty"
        else let ls = intercalate "," $ map show disjunctive
             in if isTeX
                then printf "$\\bigvee{}_{%s}$\n" ls
                else putStrLn ls

      when isTeX $ putStrLn ""
      putStr "principal conjunctive normal form: "
      if null conjunctive
        then putStrLn "empty"
        else let ls = intercalate "," $ map show conjunctive
             in if isTeX
                then printf "$\\bigwedge{}_{%s}$\n" ls
                else putStrLn ls

  when isTeX $ putStrLn "\\end{document}"
