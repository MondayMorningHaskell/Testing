{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fences where

newtype FenceValues = FenceValues { unFenceValues :: [Int] }
  deriving (Show)
newtype FenceIndex = FenceIndex { unFenceIndex :: Int }
  deriving (Eq, Num, Ord)
-- Left Index is inclusive, right index is non-inclusive 
newtype FenceInterval = FenceInterval { unFenceInterval :: (FenceIndex, FenceIndex) }
newtype FenceSolution = FenceSolution { unFenceSolution :: Int }
  deriving (Eq, Show, Ord)

largestRectangle :: FenceValues -> FenceSolution
largestRectangle values = largestRectangleAtIndices values
  (FenceInterval (FenceIndex 0, FenceIndex (length (unFenceValues values))))
 
largestRectangleAtIndices :: FenceValues -> FenceInterval -> FenceSolution
largestRectangleAtIndices
  values
  interval@(FenceInterval (leftIndex, rightIndex)) = 
    -- Base Case: Checks if left + 1 >= right
    if isBaseInterval interval
      then FenceSolution (valueAtIndex values leftIndex)
      -- Compare three cases
      else max (max middleCase leftCase) rightCase
      where
      -- Find the minimum height and its index
      (minIndex, minValue) = minimumHeightIndexValue values interval
      -- Case 1: Use the minimum index
      middleCase = FenceSolution $ (intervalSize interval) * minValue
      -- Recursive call #1
      leftCase = largestRectangleAtIndices values (FenceInterval (leftIndex, minIndex))
      -- Guard against case where there is no "right" interval
      rightCase = if minIndex + 1 == rightIndex
        then FenceSolution (minBound :: Int) -- Supply a "fake" solution that we'll ignore
        -- Recursive call #2
        else largestRectangleAtIndices values (FenceInterval (minIndex + 1, rightIndex))

valueAtIndex :: FenceValues -> FenceIndex -> Int
valueAtIndex values index = (unFenceValues values) !! (unFenceIndex index)
 
isBaseInterval :: FenceInterval -> Bool
isBaseInterval (FenceInterval (FenceIndex left, FenceIndex right)) = left + 1 >= right
 
intervalSize :: FenceInterval -> Int
intervalSize (FenceInterval (FenceIndex left, FenceIndex right)) = right - left

minimumHeightIndexValue :: FenceValues -> FenceInterval -> (FenceIndex, Int)
minimumHeightIndexValue values (FenceInterval (FenceIndex left, FenceIndex right)) =
  foldl minTuple (FenceIndex (-1), maxBound :: Int) valsInInterval
  where
    valsInInterval :: [(FenceIndex, Int)]
    valsInInterval = drop left (take right (zip (FenceIndex <$> [0..]) (unFenceValues values)))
    minTuple :: (FenceIndex, Int) -> (FenceIndex, Int) -> (FenceIndex, Int)
    minTuple old@(_, heightOld) new@(_, heightNew) =
      if heightNew < heightOld then new else old
