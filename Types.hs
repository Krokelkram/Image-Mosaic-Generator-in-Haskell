{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}
module Types where
import Text.Printf
import System.Console.CmdArgs


data Pixel = Pixel {
      pR :: Int
    , pG :: Int
    , pB :: Int
    } deriving Eq

-- contains information about an image that is useful for mosaic tile matching (only medianColor so far)
data Fingerprint = Fingerprint {
      fpFilename :: String
    , fpMedian :: Pixel
    } deriving (Show)

-- Shows contents of a pixel as string.
instance Show Pixel where
    show (Pixel r g b) = printf "%d %d %d" r g b

data Mosa = Analyse {src :: FilePath, dbPath :: FilePath}
          | Generate {from :: FilePath, out :: FilePath, dbPath_ :: FilePath, columns :: Int, rows :: Int}
            deriving (Data,Typeable,Show,Eq)

