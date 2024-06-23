module Main where

import HW4
import Control.Monad (unless)
import Data.List (sort)

-- Test clusters function with Maybe values
testClustersWithMaybe :: IO ()
testClustersWithMaybe = do
  let xs = [Just [1,2], Nothing, Just [1,2,3], Just [4,5], Just [4,5,6], Nothing, Nothing, Nothing, Nothing] :: [Maybe [Int]]
  let result = clusters xs
  print result
  let expectedClusters = sort [sort [Just [1,2], Just [4,5]], sort [Just [1,2,3], Just [4,5,6]], sort [Nothing, Nothing,Nothing, Nothing, Nothing]]
      sortedResult = sort $ map sort result
  unless (sortedResult == expectedClusters) $
    error "Test failed: clusters function returned unexpected result (Maybe)"
  putStrLn "Clusters function tests with Maybe values passed successfully."

-- Test clusters function with Double values
testClustersWithDouble :: IO ()
testClustersWithDouble = do
  let xs = [[1.0, 2.0], [1.0, 2.0, 3.0], [4.0, 5.0], [4.0, 5.0, 6.0]] :: [[Double]]
  let result = clusters xs
  print result
  let expectedClusters = sort [sort [[1.0, 2.0], [4.0, 5.0]], sort [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]]
      sortedResult = sort $ map sort result
  unless (sortedResult == expectedClusters) $
    error "Test failed: clusters function returned unexpected result (Double)"
  putStrLn "Clusters function tests with Double values passed successfully."

main :: IO ()
main = do
  testClustersWithMaybe
  testClustersWithDouble