{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}
module Types where
import Text.Printf
import System.Console.CmdArgs

-- Pixels are single color values, consisting of an R (red), G (green) and B (blue) color value
data Pixel = Pixel {
      pR :: Int
    , pG :: Int
    , pB :: Int
    } deriving Eq

-- a Fingerprint contains the information that is needed to find images that
-- fit into the mosaic. The average color of an image is sufficient for this
-- but it can easyily be expanded with more information about an image
data Fingerprint = Fingerprint {
      fpFilename :: String
    , fpMedian :: Pixel
    } deriving (Show)

-- Shows contents of a pixel as string in which the values are separated by whitespaces
instance Show Pixel where
    show (Pixel r g b) = printf "%d %d %d" r g b

-- there are two modes can be be selected by command arguments
-- 'Analyse' needs a folder to analyse (default=.) and a path to the database file (default:DB.txt)
-- 'Generate' needs an image to 'mosaicify' and a file to save the resulting image to, optionally the
-- user can specify a number of rows and columns in the mosaic
data Mosa = Analyse {src :: FilePath, dbPath :: FilePath}
          | Generate {from :: FilePath, out :: FilePath, dbPath_ :: FilePath, columns :: Int, rows :: Int}
            deriving (Data,Typeable,Show,Eq)

