module Main.Input where

import Conduit
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (isExtensionOf)

type M = ResourceT IO

isGpx :: FilePath -> Bool
isGpx f = "gpx" `isExtensionOf` f || "gpx.gz" `isExtensionOf` f

gpxInDirectoryC :: FilePath -> ConduitT a FilePath M ()
gpxInDirectoryC d = sourceDirectoryDeep False d .| filterC isGpx

expandInputC :: FilePath -> ConduitT a FilePath M ()
expandInputC i = do
    isDir <- liftIO $ doesDirectoryExist i
    isFile <- liftIO $ doesFileExist i
    case (isDir, isFile) of
        (True, _) -> gpxInDirectoryC i
        (_, True) | isGpx i -> yield i
        (_, _) -> fail $ "bad input file/dir: " <> i

expandInputsC :: ConduitT FilePath FilePath M ()
expandInputsC = awaitForever expandInputC

expandInputs :: [FilePath] -> IO [FilePath]
expandInputs is = runConduitRes $ yieldMany is .| expandInputsC .| sinkList
