module MergeSort where

import qualified Control.Monad (return)

sort :: (Ord a) => [a] -> [[a]]
sort = sort' . map return

mergePairs [] = []
mergePairs [xs] = [xs]
mergePairs (xs:ys:xss) = merge xs ys:mergePairs xss

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) =
  if x < y then x:merge xs (y:ys)
  else y:merge (x:xs) ys

sort' xss = (if length xss < 2 then id else sort' . mergePairs) xss