{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, ViewPatterns #-}
import Control.Monad
import Data.List
import Data.Maybe
import Data.Function
import qualified Data.Sequence as Seq
import qualified Data.Map as M
import qualified Data.MultiSet as SS
import System.IO

type State = [Int]
target = [1..8]++[0] :: State
target' = fromEnum target

factorials = 1 : scanl1 (*) [1..]

instance Enum State where
    fromEnum a = (\(_,_,acc) -> acc) $ foldr (\x (i,l,acc) -> (i+1,x:l,acc+(factorials!!i)*length (filter (<x) l))) (0,[],0) a
    toEnum acc = unfoldr (\(i,l,acc) ->
        if i < 0 then Nothing
        else let x = l !! (acc `div` (factorials!!i))
             in Just (x, (i-1,delete x l,acc `mod` (factorials!!i)))
            ) (8,[0..8],acc)

moves :: State -> [State]
moves s = [ map (\x -> if x == 0 then s!!pos' else if x == s!!pos' then 0 else x) s
          | d <- [-1,3,1,-3]
          , not $ pos `mod` 3 == 0 && d == (-1)
          , not $ pos `mod` 3 == 2 && d == 1
          , let pos' = pos + d
          , not $ pos' < 0 || pos' >= 9
          ]
  where
    pos = fromJust $ findIndex (==0) s

solve :: State -> IO ()
solve src = do
    let ss = if fromEnum src == target' then M.singleton 0 (-1)
             else
#ifdef BFS
                 bfs (Seq.singleton src) $ M.singleton (fromEnum src) (-1)
#else
                 astar (SS.singleton (heuristic src, src)) $ M.singleton (fromEnum src) (-1)
#endif
    if odd (inverse (delete 0 src) - inverse (delete 0 target))
       then hPrint stderr "no solution"
       else do
            hPrint stderr $ getAncestors (fromEnum target) ss
            putStrLn "digraph G {"
            forM_ (nub $ M.keys ss) $ \s -> do
                putStrLn $ show s ++ " [shape=record" ++
                    (if s == fromEnum src
                     then ",style=filled,color=orange"
                     else if s == fromEnum target
                          then ",style=filled,color=orchid"
                          else "") ++ ",label=\""++label s++"\"];"
            forM_ (filter ((/=fromEnum src) . fst) $ M.toList ss) $ \(s,p) -> do
                putStrLn $ show p ++ "->" ++ show s ++ ";"
            putStrLn "}"
  where
    label s = intercalate "|" $ map (('{':).(++"}")) $ map (intersperse '|' . concatMap show . map snd) $ transpose $ groupBy ((/=) `on` fst) $ zip (cycle [1..3]) (toEnum s :: State)
    getAncestors :: Int -> M.Map Int Int -> Int
    getAncestors s m
        | s == fromEnum src = 0
        | otherwise = 1 + getAncestors (fromJust $ M.lookup s m) m
    inverse = (\(_,acc) -> acc) . foldr (\x (l,acc) -> (x:l,acc+length(filter(<x)l))) ([],0)

bfs :: Seq.Seq State -> M.Map Int Int -> M.Map Int Int
bfs (Seq.viewl -> s Seq.:< ss) closed
    | isJust $ find (==target') suc' = closed'
    | otherwise = bfs (ss Seq.>< Seq.fromList suc) closed'
  where
    suc = filter (not . flip M.member closed . fromEnum) $ moves s
    suc' = map fromEnum suc
    num = fromEnum s
    closed' = M.union closed $ M.fromList $ zip suc' (repeat num)

heuristic :: State -> Int
heuristic x = sum . map (\(x,y) -> distance x (y-1)) . filter ((/=0) . snd) $ [0..] `zip` x
  where
    distance p q = abs (x1-x2) + abs (y1-y2)
      where
        (x1,y1) = p `divMod` 3
        (x2,y2) = q `divMod` 3

astar :: SS.MultiSet (Int, State) -> M.Map Int Int -> M.Map Int Int
astar (SS.minView -> Just ((c,s),ss)) closed
    | isJust $ find (==target') suc' = closed'
    | otherwise = astar (SS.union ss $ SS.fromList $ map (\q -> (c - heuristic s + 1 + heuristic q, q)) suc) closed'
  where
    suc = filter (not . flip M.member closed . fromEnum) $ moves s
    suc' = map fromEnum suc
    num = fromEnum s
    closed' = M.union closed $ M.fromList $ zip suc' (repeat num)

main = do
    line <- getLine
    solve $ map (read . return) line