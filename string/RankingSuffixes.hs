module RankingSuffixes (
  rank, rankTails1
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Instances
import Data.Array
import Data.Function
import Data.List
import Control.Monad.ST
import Data.Array.ST

rank :: (Ord a) => [a] -> [Int]
rank = elems . liftM2 array ((,) 0 . pred . length) (map (first snd) . concat . label . groupBy ((==) `on` fst) . sort . flip zip [0..])
  where
    label = zipWith (flip (map . flip (,))) <*> scanl (+) 0 . map length

boundRank :: [(Int,Int)] -> [Int]
boundRank xs = elems . array (0,n-1) . map (first snd) . concat . label . groupBy ((==) `on` fst) . countingSortBy (fst.fst) n . countingSortBy (succ.snd.fst) (n+1) $ xs `zip` [0..]
  where
    n = length xs
    label = zipWith (flip (map . flip (,))) <*> scanl (+) 0 . map length

rankTails1 :: (Ord a) => [a] -> [Int]
rankTails1 = liftM3 applyUntil (((and . elems).) . (.flip zip (repeat True)) . accumArray (||) False . (,) 0 . pred . length) (const $ map reorder (iterate (*2) 1)) rank
  where
    reorder = (boundRank.) . (zip <*>) . ((++ repeat (-1)).) . drop
    applyUntil p fs x = head . dropWhile (not . p) $ scanl (flip ($)) x fs

countingSortBy :: (a -> Int) -> Int -> [a] -> [a]
countingSortBy f n xs = runST $ do
  a <- newArray (0,n-1) 0 :: ST s (STUArray s Int Int)
  forM_ xs $ \x -> readArray a (f x) >>= writeArray a (f x) . succ
  forM_ [1..n-1] $ \i -> readArray a (i-1) >>= \p -> readArray a i >>= \q -> writeArray a i (p+q)
  (elems . array (0,length xs-1)) `fmap` forM (reverse xs) (\x -> do
    i <- pred `fmap` readArray a (f x)
    writeArray a (f x) i
    return (i,x))
