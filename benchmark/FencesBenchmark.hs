module Main where
 
import Criterion
import Criterion.Main (defaultMain)
import System.Random
import System.IO (writeFile)
 
import Fences
 
main :: IO ()
main = do
  [l1, l2, l3, l4, l5, l6] <- mapM 
    randomList [1, 10, 100, 1000, 10000, 100000]
  defaultMain
    [ bgroup "fences tests" 
      [ bench "Size 1 Test" $ whnf largestRectangle l1
      , bench "Size 10 Test" $ whnf largestRectangle l2
      , bench "Size 100 Test" $ whnf largestRectangle l3
      , bench "Size 1000 Test" $ whnf largestRectangle l4
      , bench "Size 10000 Test" $ whnf largestRectangle l5
      , bench "Size 100000 Test" $ whnf largestRectangle l6
      ]
    ]
 
-- Generate a list of a particular size
randomList :: Int -> IO FenceValues
randomList n = FenceValues <$> (sequence $ replicate n (randomRIO (1, 10000 :: Int)))
