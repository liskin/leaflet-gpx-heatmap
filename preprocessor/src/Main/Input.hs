module Main.Input where

import System.Directory (listDirectory, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>), isExtensionOf)

gpxInDirectory :: FilePath -> IO [FilePath]
gpxInDirectory d = (map (d </>) . filter isGpx) <$> listDirectory d

expandInputs :: [FilePath] -> IO [FilePath]
expandInputs is = mconcat <$> mapM expandInput is

expandInput :: FilePath -> IO [FilePath]
expandInput i = do
    isDir <- doesDirectoryExist i
    isFile <- doesFileExist i
    case (isDir, isFile) of
        (True, _) -> gpxInDirectory i
        (_, True) | isGpx i -> pure [i]
        (_, _) -> error $ "bad input file/dir: " <> i

isGpx :: FilePath -> Bool
isGpx f = "gpx" `isExtensionOf` f || "gpx.gz" `isExtensionOf` f
