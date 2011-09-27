module InsertionSort where

sort :: (Ord a) => [a] -> [a]
sort = flip sort' id

sort' [] f = f []
sort' (x:xs) f = sort' xs (f . insert x)
  where
    insert x [] = [x]
    insert x l@(y:ys) = if x < y then x:l else y:insert x ys