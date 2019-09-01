module Main (main) where

import Control.Monad
import System.Environment (getArgs)

import qualified Data.Vector.Unboxed as UV

import Main.Input

main :: IO ()
main = do
    args <- getArgs
    inputs <- expandInputs args
    forM_ inputs $ \(filename, parseInput) -> do
        input <- parseInput
        print (filename, map UV.length input)
