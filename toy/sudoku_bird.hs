{-
An implementation of A simple Sudoku solver from
Pearls of Functional Algorithm Design by Richard Bird
-}

import Control.Arrow
import Control.Monad
import Data.List

type Row a = [a]
type Matrix a = [Row a]
type Digit = Char
type Choices = [Digit]

rows = id
cols = transpose
boxes = map concat . concat . map transpose . group . map group
  where
    group [] = []
    group xs = uncurry (:) . second group . splitAt 3 $ xs

nodups [] = True
nodups (x:xs) = not (elem x xs) && nodups xs

choices = map (map (\d -> if d == '0' then ['1'..'9'] else [d]))

expand rows = [u ++ [l ++ [c] : r] ++ d | c <- cs]
  where
    (u, row:d) = break (any smallest) rows
    (l, cs:r) = break smallest row
    smallest = (==n) . length
    n = minimum . filter (/=1) . map length $ concat rows

search g
  | not (safe g) = []
  | complete g' = [map (map head) g']
  | otherwise = concatMap search (expand g')
  where
    g' = prune g
    complete = all (all ((==1) . length))
    safe m = all ok (rows m) && all ok (cols m) && all ok (boxes m)
    ok row = nodups [d | [d] <- row]

pruneRow row = map (remove fixed) row
  where
    fixed = [d | [d] <- row]
    remove xs ds = if length ds == 1 then ds else ds \\ xs

prune = pruneBy boxes . pruneBy cols . pruneBy rows
  where
    pruneBy way = way . map pruneRow . way

solve :: Matrix Digit -> [Matrix Digit]
solve = search . choices

main = replicateM 9 getLine >>= mapM ((putStrLn "---------" >>) . mapM_ putStrLn) . solve
