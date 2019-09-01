{-# LANGUAGE OverloadedStrings #-}

module Main.Input (Input, Point, TrackSegment, expandInputs) where

import Data.ByteString (ByteString)
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>), isExtensionOf)
import Xeno.Types (XenoException)

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lex.Fractional as B (readDecimal, readSigned)
import qualified Data.Vector.Unboxed as UV
import qualified Xeno.DOM as X

type Point = (Double, Double)
type TrackSegment = UV.Vector Point
type Input = (FilePath, IO [TrackSegment])

parseGpx :: ByteString -> Either XenoException [TrackSegment]
parseGpx = fmap parseXml . X.parse
    where
        parseXml xml =
            [ parseTrkSeg trkseg
            | trk <- X.children xml
            , X.name trk == "trk"
            , trkseg <- X.children trk
            , X.name trkseg == "trkseg"
            ]
        parseTrkSeg trkseg = UV.fromList
            [ parseTrkPt trkpt
            | trkpt <- X.children trkseg
            , X.name trkpt == "trkpt"
            ]
        parseTrkPt trkpt = (readDouble lat', readDouble lon')
            where
                attrs = X.attributes trkpt
                Just lat' = "lat" `lookup` attrs
                Just lon' = "lon" `lookup` attrs

readDouble :: ByteString -> Double
readDouble b = case B.readSigned B.readDecimal b of
    Just (d, "") -> d
    _ -> error $ "readDouble: " ++ show b

isGpx :: FilePath -> Bool
isGpx f = "gpx" `isExtensionOf` f || "gpx.gz" `isExtensionOf` f

gpxInDirectory :: FilePath -> IO [FilePath]
gpxInDirectory d = (map (d </>) . filter isGpx) <$> listDirectory d

expandInputFile :: FilePath -> Input
expandInputFile i = (i, parse . ungzipIfNeeded <$> B.readFile i)
    where
        ungzipIfNeeded
            | "gz" `isExtensionOf` i = BL.toStrict . GZip.decompress . BL.fromStrict
            | otherwise = id
        parse = either (\e -> error $ i <> ": " <> show e) id . parseGpx

expandInput :: FilePath -> IO [Input]
expandInput i = do
    isDir <- doesDirectoryExist i
    isFile <- doesFileExist i
    case (isDir, isFile) of
        (True, _) -> map expandInputFile <$> gpxInDirectory i
        (_, True) | isGpx i -> pure [expandInputFile i]
        (_, _) -> error $ "bad input file/dir: " <> i

expandInputs :: [FilePath] -> IO [Input]
expandInputs is = mconcat <$> mapM expandInput is
