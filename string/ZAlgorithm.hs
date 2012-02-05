module ZAlgorithm (
  zalgo
  ) where
import Data.Array hiding (elems)

data Queue a = Queue [a] [a] deriving (Show)

empty = Queue [] []
singleton x = Queue [x] []
snoc (Queue xs ys) x = Queue xs (x:ys)
viewl (Queue (x:xs) ys) = (x, Queue xs ys)
viewl (Queue [] ys) = viewl $ Queue (reverse ys) []
elems (Queue [] []) = []
elems q = let (h,q') = viewl q in h:elems q'

zalgo :: (Eq a) => [a] -> [Int]
zalgo xs = elems . (\(za,_,_) -> za) $ foldl f (singleton n,empty,0) [1..n-1]
  where
    n = length xs
    a = listArray (0,n-1) xs
    f (za, q, g) i
      | g >= i && h /= g-i = let x = min h (g-i) in (snoc za x, snoc q' x, g)
      | otherwise = let g' = max g i
                        l = g' + lcp g' (g'-i)
                    in (snoc za (l-i), snoc a' (l-i), l)
      where
        (h,q') = viewl q
        (_,a') = viewl za
    lcp i j
      | i == n || j == n = 0
      | a!i == a!j = 1 + lcp (i+1) (j+1)
      | otherwise = 0