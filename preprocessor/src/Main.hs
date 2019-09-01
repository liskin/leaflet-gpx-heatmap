{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Concurrent.ParallelIO.Global (parallel)
import Control.DeepSeq
import Data.Map (Map)
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
    inputs <- expandInputs files
    fmap Map.fromList $ progress "Loading GPX files" parallel $
        flip map inputs $ \(filename, parseInput) -> do
            !input <- force <$> parseInput
            pure (filename, input)

progress :: String -> ([IO a] -> IO [a]) -> ([IO a] -> IO [a])
progress what sequence' as = displayConsoleRegions $ do
    progressBar <- newProgressBar progressOpts
    sequence' $ map (<* tick progressBar) as
    where
        progressOpts = def{ pgTotal = fromIntegral (length as), pgFormat = format }
        format = what <> " :percent [:bar] :current/:total (for :elapsed, :eta remaining)"
