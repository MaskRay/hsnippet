module QuickSort where

import qualified Data.List (partition)

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort xs = let (ys, x:zs) = Data.List.partition (< head xs) xs in sort ys ++ [x] ++ sort zs