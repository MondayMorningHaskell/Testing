module Main where
 
import Criterion
import Criterion.Main (defaultMain)
import Data.Array (listArray)
import System.Random
import System.IO (writeFile)
 
import FencesFast
 
main :: IO ()
main = do
  [l1, l2, l3, l4, l5, l6] <- mapM 
    randomList [1, 10, 100, 1000, 10000, 100000]
  let l7 = sortedList
  defaultMain
    [ bgroup "fences tests" 
      [ bench "Size 1 Test" $ whnf largestRectangle l1
      , bench "Size 10 Test" $ whnf largestRectangle l2
      , bench "Size 100 Test" $ whnf largestRectangle l3
      , bench "Size 1000 Test" $ whnf largestRectangle l4
      , bench "Size 10000 Test" $ whnf largestRectangle l5
      , bench "Size 100000 Test" $ whnf largestRectangle l6
      , bench "Size 100000 Test (sorted)" $ whnf largestRectangle l7
      ]
    ]
 
randomList :: Int -> IO FenceValues
randomList n = FenceValues . mkListArray <$> 
  (sequence $ replicate n (randomRIO (1, 10000 :: Int)))
  where
    mkListArray vals = listArray (0, (length vals) - 1) vals
 
sortedList :: FenceValues
sortedList = FenceValues $ listArray (0, 99999) [1..100000]
