module Main.Input where

import Conduit
import Data.ByteString (ByteString)
import Data.Conduit.Zlib (ungzip)
import Data.Monoid
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (isExtensionOf)

type M = ResourceT IO

isGpx :: FilePath -> Bool
isGpx f = "gpx" `isExtensionOf` f || "gpx.gz" `isExtensionOf` f

gpxInDirectoryC :: FilePath -> ConduitT a FilePath M ()
gpxInDirectoryC d = sourceDirectoryDeep False d .| filterC isGpx

processFileC :: ConduitT ByteString o M () -> FilePath -> ConduitT i o M ()
processFileC c i
    | "gpx" `isExtensionOf` i = sourceFile i .| c
    | "gpx.gz" `isExtensionOf` i = sourceFile i .| ungzip .| c
    | otherwise = fail $ "bad input file/dir: " <> i

processInputC :: ConduitT ByteString o M () -> FilePath -> ConduitT i o M ()
processInputC c i = do
    isDir <- liftIO $ doesDirectoryExist i
    isFile <- liftIO $ doesFileExist i
    case (isDir, isFile) of
        (True, _) -> gpxInDirectoryC i .| awaitForever (processFileC c)
        (_, True) -> processFileC c i
        (_, _) -> fail $ "nonexistent file/dir: " <> i

processInputsC :: ConduitT ByteString o M () -> ConduitT FilePath o M ()
processInputsC c = awaitForever (processInputC c)

processInputs :: Monoid a => ConduitT ByteString a M () -> [FilePath] -> IO a
processInputs c is = runConduitRes $ yieldMany is .| processInputsC c .| foldC
