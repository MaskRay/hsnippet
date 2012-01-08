module BurrowsWheelerTransform (
  bwt, ibwt
  ) where
import Control.Arrow
import Control.Monad
import Control.Monad.Instances
import Data.Array
import Data.List
import Data.Maybe
import RankingSuffixes

invert n = elems . array (0,n-1) . flip zip [0..]

uniqueRank :: (Ord a) => [a] -> [Int]
uniqueRank = elems . liftM2 array ((,) 0 . pred . length) (map (first snd) . flip zip [0..] . sort . flip zip [0..])

bwt :: (Ord a) => [a] -> ([a], Int)
bwt xs = (map (ys!) $ zs, fromJust $ findIndex (==0) zs)
  where
    n = length xs
    zs = map modPred . invert n . rank . take n . rankTails1 $ xs++xs
    ys = listArray (0,n-1) xs
    modPred i = if i == 0 then n-1 else i-1

ibwt :: (Ord a) => [a] -> Int -> [a]
ibwt xs k = take n . map (ys!) $ iterate (r2i!) k
  where
    n = length xs
    ys = listArray (0,n-1) xs
    r2i = listArray (0,n-1) . invert n $ uniqueRank xs