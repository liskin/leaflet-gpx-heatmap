{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Concurrent.ParallelIO.Global (parallel)
import Control.DeepSeq
import Data.List (sortBy)
import Data.Map (Map)
import Data.Ord (comparing)
import System.Console.AsciiProgress
import System.Environment (getArgs)

import qualified Data.Map as Map

import Main.Input

main :: IO ()
main = do
    inputs <- loadInputs =<< getArgs
    print $ Map.size inputs

loadInputs :: [FilePath] -> IO (Map FilePath [TrackSegment])
loadInputs files = do
    inputs <- sortBy (comparing fst) <$> expandInputs files
    fmap Map.fromList $ progressParallel "Loading GPX files" $
        flip map inputs $ \(filename, parseInput) -> do
            !input <- force <$> parseInput
            pure (filename, input)

progressParallel :: String -> [IO a] -> IO [a]
progressParallel what as = displayConsoleRegions $ do
    progress <- newProgressBar def{ pgTotal = fromIntegral (length as), pgFormat = format }
    parallel $ map (<* tick progress) as
    where
        format = what <> " :percent [:bar] :current/:total (for :elapsed, :eta remaining)"
