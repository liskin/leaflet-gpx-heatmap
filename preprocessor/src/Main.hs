module Main (main) where

import Control.Concurrent.ParallelIO.Global (parallel_)
import Data.List (sortBy)
import Data.Ord (comparing)
import System.Environment (getArgs)

import qualified Data.Vector.Unboxed as UV

import Main.Input

main :: IO ()
main = do
    args <- getArgs
    inputs <- sortBy (comparing fst) <$> expandInputs args
    parallel_ $ flip map inputs $ \(filename, parseInput) -> do
        input <- parseInput
        sum (map UV.length input) `seq` filename `seq` putStrLn "."
