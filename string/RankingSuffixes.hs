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

rank :: (Ord a) => [a] -> [Int]
rank = elems . liftM2 array ((,) 0 . pred . length) (map (first snd) . concat . label . groupBy ((==) `on` fst) . sort . flip zip [0..])
  where
    label = zipWith (flip (map . flip (,))) <*> scanl (+) 0 . map length

rankTails1 :: (Ord a) => [a] -> [Int]
rankTails1 = liftM3 applyUntil (((and . elems).) . (.flip zip (repeat True)) . accumArray (||) False . (,) 0 . pred . length) (const $ map reorder (iterate (*2) 1)) rank
  where
    reorder = (rank.) . (zip <*>) . ((++ repeat (-1)).) . drop
    applyUntil p fs x = head . dropWhile (not . p) $ scanl (flip ($)) x fs
