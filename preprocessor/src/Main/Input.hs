{-# LANGUAGE OverloadedStrings #-}

module Main.Input where

import Conduit
import Control.Monad
import Data.ByteString (ByteString)
import Data.Conduit.Zlib (ungzip)
import Data.Default
import Data.XML.Types (Event)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (isExtensionOf)
import Text.XML (Name(nameLocalName))
import Text.XML.Stream.Parse

import qualified Data.Text as T

type M = ResourceT IO

data InputEvent = EndTrkSeg | Point{ ptLat :: Double, ptLon :: Double }
    deriving (Show, Eq, Ord)

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

processGpxC :: ConduitT ByteString InputEvent M ()
processGpxC = parseBytes def .| parseGpxC

parseGpxC :: ConduitT Event InputEvent M ()
parseGpxC = do
    force "<gpx> expected" $ tagIgnoreAttrs (unqual "gpx") $ do
        void $ many' $ tagIgnoreAttrs (unqual "trk") $ do
            manyYield' $ tagIgnoreAttrs (unqual "trkseg") $ do
                manyYield' $ tag' (unqual "trkpt") trkPkAttrs $ \pt -> do
                    many_ ignoreAnyTreeContent
                    pure pt
                pure EndTrkSeg
    where
        unqual t = matching ((t ==) . nameLocalName)
        trkPkAttrs = do
            lat <- read . T.unpack <$> requireAttr "lat"
            lon <- read . T.unpack <$> requireAttr "lon"
            ignoreAttrs
            pure Point{ ptLat = lat, ptLon = lon }
