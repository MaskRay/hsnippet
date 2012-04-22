{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction, RankNTypes, ScopedTypeVariables, TupleSections, TypeFamilies #-}

import Control.Arrow
import Control.Monad
import Control.Monad.Instances
import Data.List

fibonacci = 1 : scanl (+) 2 fibonacci

class BitStream a where
    empty :: a
    isEmpty :: a -> Bool
    put :: Bool -> a -> a
    get :: a -> (Bool, a)
    toList :: a -> [Bool]

instance BitStream [Bool] where
    empty = []
    isEmpty = null
    put = flip (++) . return
    get = liftM2 (,) head tail
    toList = id

class FibonacciCoding a where
    encode :: forall b. BitStream b => a -> b
    decode :: forall b. BitStream b => b -> (a, b)

instance FibonacciCoding Int where
    encode n = put True . go n $ reverse $ takeWhile (<=n) fibonacci
      where
        go n [] = empty
        go n (x:xs)
            | x <= n = put True $ go (n-x) xs
            | otherwise = put False $ go n xs
    decode s = first (+ fromEnum hd) $ go (tail fibonacci) hd tl
      where
        (hd, tl) = get s
        go (f:fs) y xs
          | y && x = (0, xs')
          | not x = go fs x xs'
          | x = first (+f) $ go fs x xs'
          where
            (x, xs') = get xs
