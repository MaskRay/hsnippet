import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List
import Data.Maybe
import Foreign.Marshal.Utils
import System.IO
import System.Environment
import System.Exit
import Text.ParserCombinators.Parsec hiding ((<|>))
import Text.Printf

type Table = [(Char, Bool)]
  
data Tree = Equiv Tree Tree | Imply Tree Tree | Not Tree | And Tree Tree | Or Tree Tree | Var Char deriving (Show)

parser :: Int -> WriterT String (CharParser st) Tree
parser d = lift spaces *>
           (if d < 4
            then go >>= \x -> lift (return (foldl1 ([Equiv,Imply,Or,And]!!d) x))
            else liftM Not (lift (char '!') >> parser d) <|> (lift alphaNum >>= \c -> tell [c] >> return (Var c)) <|> (lift (char '(') *> parser 0 <* lift (char ')'))
           ) <* lift spaces
  where
    go :: WriterT String (CharParser st) [Tree]
    go = do
      x <- parser (d+1)
      m <- lift $ optionMaybe (string (["<->","->","|","&"]!!d))
      if m == Nothing
        then return [x]
        else fmap (x:) go

eval :: Tree -> Reader Table Bool
eval (Var c) = fromJust <$> asks (lookup c)
eval (Not x) = not <$> eval x
eval (And x y) = (&&) <$> eval x <*> eval y
eval (Or x y) = (||) <$> eval x <*> eval y
eval (Imply x y) = ((||) . not) <$> eval x <*> eval y
eval (Equiv x y) = (==) <$> eval x <*> eval y

printDisjunctive :: Bool -> [(Bool, Int)] -> IO ()
printDisjunctive isTeX truth = do
  let t = map snd $ filter fst $ sort truth
  putStr "principal disjunctive normal form: "
  if null t
    then putStrLn "empty"
    else let ls = intercalate "," $ map show t
         in if isTeX
            then printf "$\\bigvee{}_{%s}$\n" ls
            else putStrLn ls

printConjunctive :: Bool -> [(Bool, Int)] -> IO ()
printConjunctive isTeX truth = do
  let f = reverse $ map (((length truth-1)-) . snd) $ filter (not . fst) $ sort truth
  putStr "principal conjunctive normal form: "
  if null f
    then putStrLn "empty"
    else let ls = intercalate "," $ map show f
         in if isTeX
            then printf "$\\bigwedge{}_{%s}$\n" ls
            else putStrLn ls

main = do
  args <- getArgs
  let isTeX = any (`elem` ["-t", "--tex"]) args
  let isRev = any (`elem` ["-r", "--reverse"]) args
  when isTeX $ putStrLn "\\documentclass[12pt]{article}\n\\parindent 0em\n\\pagestyle{empty}\n\\begin{document}"
  truth <- if isRev
           then do
             vars <- getLine
             replicateM (2^length vars) $ words <$> getLine >>= \xs -> let xs' = map read xs :: [Int] in return (toBool (last xs'), foldl ((+) . (*2)) 0 (init xs'))
    
           else do
             let calate = intercalate $ if isTeX then " & " else " "
             getLine >>= \line -> case parse (runWriterT (parser 0 <* lift eof)) "" line of
               Left e -> hPrint stderr e >> exitFailure
               Right (t, var') -> do
                 let var = nub $ sort var'
                     truth = zip (map (runReader (eval t) . zip var) (mapM (const [False,True]) [1..length var])) [0..]
           
                 if isTeX
                   then do
                   printf "\n\\begin{tabular}{%s}\n" $ replicate (length var+1) 'l'
                   putStrLn $ (++"\\\\\n\\hline") . intercalate " & " $ map (("\\textit{"++) . (++"}")) $ map pure var ++ ["result"]
                   else putStrLn $ unwords $ map pure var ++ [line]
         
                 forM_ (zip truth $ mapM (const [False,True]) [1..length var]) $ \(b,vs) -> do
                   putStr $ calate $ map (show . fromBool) $ vs ++ [fst b]
                   if isTeX
                     then putStrLn "\\\\"
                     else putStrLn ""
         
                 when isTeX $ putStrLn "\\end{tabular}\n"
                 putStr "propositional variables: "
                 if isTeX
                   then putStrLn $ ('$':).(++"$") $ intercalate "\\;" $ map pure var
                   else putStrLn $ intersperse ' ' var
                 return truth
         
  when isTeX $ putStrLn ""
  printDisjunctive isTeX truth
  when isTeX $ putStrLn ""
  printConjunctive isTeX truth
  when isTeX $ putStrLn "\\end{document}"
