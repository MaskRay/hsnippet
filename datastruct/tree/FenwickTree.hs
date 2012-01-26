{-
 - fenwick is a MArray with bounds (0,n-1)
 -}
add :: (Ix i, Bits i, Num e, MArray a e m) => a i e -> i -> e -> m ()
add !fenwick !x !delta = getBounds fenwick >>= \ !bnds -> forM_ (takeWhile (inRange bnds) (iterate (\ !i -> i .|. (i+1)) x)) $ \ !i -> readArray fenwick i >>= \ !fi -> writeArray fenwick i (fi+delta)

getSum :: (Ix i, Bits i, Num e, MArray a e m) => a i e -> i -> m e
getSum !fenwick !x = sum `liftM` mapM (readArray fenwick . (subtract 1)) (takeWhile (>0) $ iterate (\ !i -> i .&. (i-1)) x)
