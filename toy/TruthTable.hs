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

parser :: Int -> WriterT String (CharParser st) Tree
parser d = if d < 4
           then go >>= \x -> lift (return (foldl1 ([Equiv,Imply,Or,And]!!d) x))
           else (lift (char '!') >> parser d >>= return . Not) <|> (lift alphaNum >>= \c -> tell [c] >> return (Var c)) <|> (lift (char '(') *> parser 0 <* lift (char ')'))
  where
    go :: WriterT String (CharParser st) [Tree]
    go = do
      x <- parser (d+1)
      m <- lift $ optionMaybe (string (["<->","->","|","&"]!!d))
      if m == Nothing
        then return [x]
        else go >>= return . (x:)
-- parser (d+1) >>= \x -> optionMaybe (lift (string (["<->","->","|","&"]!!d)) *> go) >>= \y -> return (maybe [x] (x:) y)

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
  getLine >>= \line -> case parse (runWriterT (parser 0)) "" line of
    Left e -> print e
    Right (t, var') -> do
      let var = nub $ sort var'
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
