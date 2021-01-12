{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FencesPractice where

import Data.Array
import Data.Ix (range)

-- TODO: Fill in the undefined definitions in this file!
--       Use Test Driven Development, in the file test/FencesPracticeTests.hs!

newtype FenceValues = FenceValues { unFenceValues :: Array Int Int }
  deriving (Show, Eq)
newtype FenceIndex = FenceIndex { unFenceIndex :: Int }
  deriving (Eq, Num, Ord, Show)
-- Left Index is inclusive, right index is non-inclusive 
newtype FenceInterval = FenceInterval { unFenceInterval :: (FenceIndex, FenceIndex) }
  deriving (Show, Eq)
newtype FenceSolution = FenceSolution { unFenceSolution :: Int }
  deriving (Eq, Show, Ord)

largestRectangle :: FenceValues -> FenceSolution
largestRectangle values = largestRectangleAtIndices values 
  (buildSegmentTree (unFenceValues values))
  (FenceInterval (FenceIndex 0, FenceIndex (length (unFenceValues values))))
 
-- TODO Fill in this definition!
largestRectangleAtIndices :: FenceValues -> SegmentTreeNode -> FenceInterval -> FenceSolution
largestRectangleAtIndices = undefined

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
  deriving (Show, Eq)

buildSegmentTree :: Array Int Int -> SegmentTreeNode
buildSegmentTree ints = buildSegmentTreeTail 
  ints 
  (FenceInterval ((FenceIndex 0), (FenceIndex (length (elems ints)))))
 
-- TODO Fill in this definition!
buildSegmentTreeTail :: Array Int Int -> FenceInterval -> SegmentTreeNode
buildSegmentTreeTail = undefined

-- Get minimum val and index, but account for empty case.
valFromNode :: SegmentTreeNode -> (Int, FenceIndex)
valFromNode EmptyNode = (maxBound :: Int, FenceIndex (-1))
valFromNode n@ValueNode{} = (value n, minIndex n)

-- TODO Fill in this definition!
minimumHeightIndexValue :: FenceValues -> SegmentTreeNode -> FenceInterval -> (FenceIndex, Int)
minimumHeightIndexValue = undefined
