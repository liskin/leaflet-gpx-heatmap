module Main (main) where

import Conduit
import System.Environment (getArgs)

import Main.Input

main :: IO ()
main = do
    args <- getArgs
    parseInputs processData args

processData :: ConduitT InputEvent () M ()
processData = filterC (== EndTrkSeg) .| mapM_C (liftIO . print)
