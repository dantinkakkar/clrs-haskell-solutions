module Problem4__1_5 where

import Data.List

accumulate :: (Integer,Integer) -> Integer -> (Integer,Integer)
accumulate (a, b) c = ((max a nMax),nMax) where nMax = if (c > (b + c)) then c else (b + c)

maxContiguousSubArray :: [Integer] -> (Integer,Integer)
maxContiguousSubArray [] = (0,0)
maxContiguousSubArray (x:xs) = foldl' accumulate (x,x) xs
