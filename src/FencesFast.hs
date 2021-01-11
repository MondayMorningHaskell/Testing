{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FencesFast where

import Data.Array
import Data.Ix (range)

newtype FenceValues = FenceValues { unFenceValues :: Array Int Int }
newtype FenceIndex = FenceIndex { unFenceIndex :: Int }
  deriving (Eq, Num, Ord)
-- Left Index is inclusive, right index is non-inclusive 
newtype FenceInterval = FenceInterval { unFenceInterval :: (FenceIndex, FenceIndex) }
newtype FenceSolution = FenceSolution { unFenceSolution :: Int }
  deriving (Eq, Show, Ord)

largestRectangle :: FenceValues -> FenceSolution
largestRectangle values = largestRectangleAtIndices values 
  (buildSegmentTree (unFenceValues values))
  (FenceInterval (FenceIndex 0, FenceIndex (length (unFenceValues values))))
 
largestRectangleAtIndices :: FenceValues -> SegmentTreeNode -> FenceInterval -> FenceSolution
largestRectangleAtIndices
  values
  tree
  interval@(FenceInterval (leftIndex, rightIndex)) = 
    -- Base Case: Checks if left + 1 >= right
    if isBaseInterval interval
      then FenceSolution (valueAtIndex values leftIndex)
      -- Compare three cases
      else max (max middleCase leftCase) rightCase
      where
      -- Find the minimum height and its index
      (minIndex, minValue) = minimumHeightIndexValue values tree interval
      -- Case 1: Use the minimum index
      middleCase = FenceSolution $ (intervalSize interval) * minValue
      -- Recursive call #1
      leftCase = largestRectangleAtIndices values tree (FenceInterval (leftIndex, minIndex))
      -- Guard against case where there is no "right" interval
      rightCase = if minIndex + 1 == rightIndex
        then FenceSolution (minBound :: Int)
        else largestRectangleAtIndices values tree (FenceInterval (minIndex + 1, rightIndex))

valueAtIndex :: FenceValues -> FenceIndex -> Int
valueAtIndex values index = (unFenceValues values) ! (unFenceIndex index)
 
isBaseInterval :: FenceInterval -> Bool
isBaseInterval (FenceInterval (FenceIndex left, FenceIndex right)) = left + 1 >= right
 
intervalSize :: FenceInterval -> Int
intervalSize (FenceInterval (FenceIndex left, FenceIndex right)) = right - left

data SegmentTreeNode = ValueNode
  { fromIndex :: FenceIndex
  , toIndex :: FenceIndex
  , value :: Int
  , minIndex :: FenceIndex
  , leftChild :: SegmentTreeNode
  , rightChild :: SegmentTreeNode
  }
  | EmptyNode

buildSegmentTree :: Array Int Int -> SegmentTreeNode
buildSegmentTree ints = buildSegmentTreeTail 
  ints 
  (FenceInterval ((FenceIndex 0), (FenceIndex (length (elems ints)))))
 
buildSegmentTreeTail :: Array Int Int -> FenceInterval -> SegmentTreeNode
buildSegmentTreeTail array
  (FenceInterval (wrappedFromIndex@(FenceIndex fromIndex), wrappedToIndex@(FenceIndex toIndex)))
  | fromIndex + 1 == toIndex = ValueNode 
      { fromIndex = wrappedFromIndex
      , toIndex = wrappedToIndex
      , value = array ! fromIndex
      , minIndex = wrappedFromIndex
      , leftChild = EmptyNode
      , rightChild = EmptyNode
      }
  | fromIndex < toIndex = ValueNode 
    { fromIndex = wrappedFromIndex
    , toIndex = wrappedToIndex
    , value = newValue
    , minIndex = newIndex
    , leftChild = leftChild
    , rightChild = rightChild
    }
  | otherwise = EmptyNode
  where 
    average = (fromIndex + toIndex) `quot` 2
    -- Recursive Calls
    leftChild = buildSegmentTreeTail 
      array (FenceInterval (wrappedFromIndex, (FenceIndex average)))
    rightChild = buildSegmentTreeTail 
      array (FenceInterval ((FenceIndex average), wrappedToIndex))
    leftCase = valFromNode leftChild
    rightCase = valFromNode rightChild
    currentCase = (array ! fromIndex, wrappedFromIndex)
    (newValue, newIndex) = min (min leftCase rightCase) currentCase

-- Get minimum val and index, but account for empty case.
valFromNode :: SegmentTreeNode -> (Int, FenceIndex)
valFromNode EmptyNode = (maxBound :: Int, FenceIndex (-1))
valFromNode n@ValueNode{} = (value n, minIndex n)

minimumHeightIndexValue :: FenceValues -> SegmentTreeNode -> FenceInterval -> (FenceIndex, Int)
minimumHeightIndexValue values tree 
  originalInterval@(FenceInterval (FenceIndex left, FenceIndex right)) =
  case tree of
    EmptyNode -> (FenceIndex (-1), maxBound :: Int)
    ValueNode
      { fromIndex = FenceIndex nFromIndex
      , toIndex = FenceIndex nToIndex
      , value = nValue
      , minIndex = nMinIndex
      , leftChild = nLeftChild
      , rightChild = nRightChild} -> if left == nFromIndex && right == nToIndex 
        then (nMinIndex, nValue)
        else if right < average 
          then minimumHeightIndexValue values nLeftChild originalInterval
          else if left >= average
            then minimumHeightIndexValue values nRightChild originalInterval
            else minTuple leftResult rightResult
          where
            average = (nFromIndex + nToIndex) `quot` 2
            leftResult = minimumHeightIndexValue values nLeftChild
              (FenceInterval (FenceIndex left, FenceIndex average))
            rightResult = minimumHeightIndexValue values nRightChild
              (FenceInterval (FenceIndex average, FenceIndex right))
            minTuple :: (FenceIndex, Int) -> (FenceIndex, Int) -> (FenceIndex, Int)
            minTuple old@(_, heightOld) new@(_, heightNew) =
              if heightNew < heightOld then new else old
