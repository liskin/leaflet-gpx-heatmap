{-# LANGUAGE OverloadedStrings #-}

module Main.Input (InputEvent(..), M, parseInputs) where

import Conduit
import Control.Monad
import Data.ByteString (ByteString)
import Data.Conduit.Zlib (ungzip)
import Data.Default
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (isExtensionOf)
import Text.XML (Name(nameLocalName))
import Text.XML.Stream.Parse

import qualified Data.Text as T

type M = ResourceT IO

data InputEvent = EndTrkSeg | Point{ ptLat :: Double, ptLon :: Double }
    deriving (Show, Eq, Ord)

parseGpxC :: ConduitT ByteString InputEvent M ()
parseGpxC = parseBytes def .| parseXmlC
    where
        parseXmlC = force "<gpx> expected" $ tagIgnoreAttrs (unqual "gpx") $ do
            void $ many' $ tagIgnoreAttrs (unqual "trk") $ do
                manyYield' $ tagIgnoreAttrs (unqual "trkseg") $ do
                    manyYield' $ tag' (unqual "trkpt") trkPkAttrs $ \pt -> do
                        many_ ignoreAnyTreeContent
                        pure pt
                    pure EndTrkSeg
        unqual t = matching ((t ==) . nameLocalName)
        trkPkAttrs = do
            lat <- read . T.unpack <$> requireAttr "lat"
            lon <- read . T.unpack <$> requireAttr "lon"
            ignoreAttrs
            pure Point{ ptLat = lat, ptLon = lon }

parseFileC :: FilePath -> ConduitT i InputEvent M ()
parseFileC i = sourceFile i .| ungzipIfNeeded .| parseGpxC
    where
        ungzipIfNeeded
            | "gz" `isExtensionOf` i = ungzip
            | otherwise = awaitForever yield

isGpx :: FilePath -> Bool
isGpx f = "gpx" `isExtensionOf` f || "gpx.gz" `isExtensionOf` f

gpxInDirectoryC :: FilePath -> ConduitT a FilePath M ()
gpxInDirectoryC d = sourceDirectoryDeep False d .| filterC isGpx

parseInputC :: FilePath -> ConduitT i InputEvent M ()
parseInputC i = do
    isDir <- liftIO $ doesDirectoryExist i
    isFile <- liftIO $ doesFileExist i
    case (isDir, isFile) of
        (True, _) -> gpxInDirectoryC i .| awaitForever parseFileC
        (_, True) | isGpx i -> parseFileC i
        (_, _) -> fail $ "bad input file/dir: " <> i

parseInputsC :: ConduitT FilePath InputEvent M ()
parseInputsC = awaitForever parseInputC

parseInputs :: Monoid a => ConduitT InputEvent a M () -> [FilePath] -> IO a
parseInputs c is = runConduitRes $ yieldMany is .| parseInputsC .| c .| foldC
