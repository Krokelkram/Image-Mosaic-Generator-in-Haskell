{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Main where
import Text.Printf
import System.Environment
import System.Directory
import Data.List
import Data.Char
import Control.Parallel
import Control.Monad.ST.Strict
import Data.Array.ST
import qualified Data.ByteString.Lazy.Char8 as BS

class Image a s where
    get :: a -> (Int, Int) -> ST s Pixel
    set :: a -> (Int, Int) ->  Pixel -> ST s ()
    width :: a -> Int
    height :: a -> Int

data PPMImage s = PPMImage {
    ppmArray :: STArray s (Int,Int) Pixel
    }

data SubImage s = SubImage {
     parent :: PPMImage s
    ,offsetX :: Int
    ,offsetY :: Int
    }

instance Image (PPMImage s) s where
    get img (x,y) = do
      readArray (ppmArray img) (x,y) 
    set img (x,y) px = do
      writeArray (ppmArray img) (x,y) px
    -- just to define them
    width _  = 42
    height _ = 42

instance Image (SubImage s) s where
    get (SubImage parent x' y') (x,y) = 
        let (rx, ry) = (x'+x , y'+y)
        in get parent (rx,ry)
    set (SubImage parent x' y') (x,y) =
        let (rx, ry) = (x'+x , y'+y)
        in set parent (rx,ry)
    width _  = 42
    height _ = 42
      


data Pixel = Pixel {
      pR :: Int
    , pG :: Int
    , pB :: Int
    } deriving Eq

-- Shows contents of a pixel as string.
instance Show Pixel where
    show (Pixel r g b) = printf "%d %d %d" r g b


data BadImage = BadImage {
      imageWidth  :: Int
    , imageHeight :: Int
    , imageData   :: [Pixel]
    , imageName   :: String
    } deriving (Show, Eq)

data Fingerprint = Fingerprint {
      fpFilename :: String
    , fpMedian :: Pixel
    } deriving (Show)


myFirstArray:: ST s (Int,Int)
myFirstArray = do
  arr <- newArray ((1,1), (10,10)) 99 :: ST s (STArray s (Int,Int) Int)
  writeArray arr (7,8) 123
  a <- readArray arr (2,2)
  b <- readArray arr (7,8)
  return (a,b)


start ::[String] -> IO ()
start ("foo":_) = do
  print $ runST myFirstArray
start ("analyse":arg:_) = do
  dirContent <- getDirectoryContents arg
  images     <- mapM readPPM (filter (isSuffixOf ".ppm") dirContent)
  writeStats images
  
start ("generate":arg:_) = do
  putStrLn "Image will be generated here..."
  originalImg <- readPPM arg
  db <- readFile "DB.txt"
  let fingerprints = map dbLine2Fingerprint (lines db)
  print fingerprints
  return ()
start _ = putStrLn "Unknown parameter"

dbLine2Fingerprint :: String -> Fingerprint
dbLine2Fingerprint ln = Fingerprint fn (Pixel (read r) (read g) (read b))
    where (fn:r:g:b:_) = words ln


main :: IO ()
main = start =<< getArgs


-- Writes statistics about all images in a list to file.
writeStats :: [BadImage] -> IO ()
writeStats files = do
    writeFile "DB.txt" ""
    mapM_ forEachFile files
  where forEachFile img = do
            let dat = printf "%s %s\n" (imageName img) (show (medianColor img))
            appendFile "DB.txt" dat
          

-- Takes a list of strings (representing lines of a ppm file) and turns them
-- into a list of pixels.
str2pix :: [Int] -> [Pixel]
str2pix []         = []
str2pix (r:g:b:xs) =  Pixel r g b : (str2pix xs)


-- Takes a filename and opens it as a ppm image.
readPPM :: String -> IO BadImage
readPPM fn = do 
    c <- BS.readFile fn
    let content = BS.lines c
        [w,h]   = map (read . BS.unpack) $ BS.words (content !! 1)
        pixel   = readInts $ BS.unwords $ drop 3 content
    return $ BadImage {
           imageWidth  = w
         , imageHeight = h
         , imageData   = str2pix pixel
         , imageName   = fn }
  where readInts s =
            case BS.readInt s of
                Nothing       -> []
                Just (v,rest) -> v : readInts (next rest)
        next s = BS.dropWhile isSpace s

-- Writes a ppm image under a given filename.
writePPM :: String -> BadImage -> IO()
writePPM fn (BadImage w h dat _) = do 
    writeFile fn $ printf "P3\n%d %d\n255\n" w h
    appendFile fn (unwords $ map show dat)


-- Calculates the median color of an image.
medianColor :: BadImage -> Pixel
medianColor (BadImage width height pixel _) = 
    Pixel (average pR) (average pG) (average pB)
  where average comp = sum (map comp pixel) `div` (width * height)

