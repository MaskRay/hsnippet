{-
An implementation of Planning solves the Rush Hour problem from
Pearls of Functional Algorithm Design by Richard Bird

grid
1,2,3,4,5,6
8,9,10,11,12,13
15,16,17,18,19,20
22,23,24,25,26,27
29,30,31,32,33,34
36,37,38,39,40,41

20 is the exit cell

(g1 :: State) represents the initial state
-}

import Control.Monad
import Data.List.Ordered (union, minus)

type Cell = Int
type State = [(Cell, Cell)]
type Vehicle = Int
type Move = (Vehicle, Cell)
type Path = ([Move],State,[Move])

solve, solve' :: State -> Maybe [Move]
solve g = psearch [] [] [([],g,goalmoves g)]
solve' g = bfs [] [] [([],g)]

psearch :: (MonadPlus m) => [State] -> [Path] -> [Path] -> m [Move]
psearch closed [] [] = mzero
psearch closed rs [] = psearch closed [] rs
psearch closed rs (p@(ms,g,plan):ps)
  | solved g = return $ reverse ms
  | elem g closed = psearch closed rs ps
  | otherwise = psearch (g:closed) (bsuccs p++rs) (asuccs p++ps)
  where
    asuccs (ms,q,plan) = [(ms++[m], move q m, plan ) | m:plan <- newplans q plan]
    bsuccs (ms,q,_) = [(ms++[m], q', goalmoves q') | m <- moves q, let q' = move q m]

bfs closed [] [] = mzero
bfs closed rs [] = bfs closed [] rs
bfs closed rs (p@(ms,g):ps)
  | solved g = return $ reverse ms
  | elem g closed = bfs closed rs ps
  | otherwise = bfs (g:closed) (succs p++rs) ps
  where
    succs (ms,q) = [(ms++[m], move q m) | m <- moves q]

newplans :: State -> [Move] -> [[Move]]
newplans g [] = []
newplans g (m:ms) = mkplans (expand m++ms)
  where
    mkplans ms@(m:_)
      | elem m (moves g) = [ms]
      | otherwise = concat [ mkplans (pms++ms)
                           | pms <- premoves m
                           , all (`notElem` ms) pms
                           ]
    expand :: Move -> [Move]
    expand (v,c)
      | r > f-7 = if c > f then [(v,p) | p <- [f+1..c]]
                  else [(v,p) | p <- [r-1,r-2..c]]
      | otherwise = if c > f then [(v,p) | p <- [f+7,f+14..c]]
                    else [(v,p) | p <- [r-7,r-14..c]]
      where
        (r,f) = g!!v
    blocker :: Cell -> (Vehicle,(Cell,Cell))
    blocker c = go (zip [0..] g)
      where
        go ((v,i):vis) = if covers i then (v,i) else go vis
        covers (r,f) = r <= c && c <= f && (r > f-7 || (c-r)`mod`7 == 0)
    premoves :: Move -> [[Move]]
    premoves (v,c) = freeingmoves c (blocker c)

moves :: State -> [Move]
moves g = [(v,c) | (v,i) <- zip [0..] g
                 , c <- adjs i, elem c fs]
  where
    fs = allcells `minus` foldr (union . fillcells) [] g
    adjs (r,f) = if r > f-7 then [f+1,r-1] else [f+7,r-7]

freeingmoves :: Cell -> (Vehicle,(Cell,Cell)) -> [[Move]]
freeingmoves c (v,(r,f))
  | r > f-7 = [[(v,j) | j <- [f+1..c+n]] | c+n < k+7] ++ [[(v,j) | j <- [r-1, r-2..c-n]] | c-n > k]
  | otherwise = [[(v,j) | j <- [r-7,r-14..c-m]] | c-m > 0] ++ [[(v,j) | j <- [f+7,f+14..c+m]] | c+m < 42]
  where
    (k,m,n) = (f-f`mod`7, f-r+7, f-r+1)

goalmoves :: State -> [Move]
goalmoves g = [(0,c) | c <- [snd (head g)+1..20]]

move :: State -> Move -> [Move]
move g (v,c) = g1++adjust i c:g2
  where
    (g1,i:g2) = splitAt v g
    adjust (r , f ) c
      | r > f-7 = if c > f then (r+1, c) else (c, f-1)
      | otherwise = if c < r then (c, f-7) else (r+7, c)

allcells = concat [[i..i+5] | i <- [1,8..36]]
fillcells (r,f) = if r > f-7 then [r..f] else [r,r+7..f]
solved g = snd (head g) == 20
g1 = [(17, 18), (1, 15), (2, 9), (3, 10), (4, 11), (5, 6), (12, 19), (13, 27), (24, 26), (31, 38), (33, 34), (36, 37), (40, 41)] :: State

main = print $ solve g1