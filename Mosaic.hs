{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Main where
import Text.Printf
import System.Environment
import System.Directory
import Data.List
import Data.Char
import Control.Parallel
import Control.Monad.ST.Lazy
import Data.Array.ST
import qualified Data.ByteString.Lazy.Char8 as BS

class Image a s where
    get :: a -> (Int, Int) -> ST s Pixel
    set :: a -> (Int, Int) ->  Pixel -> ST s ()
    width :: a -> Int
    height :: a -> Int

data PPMImage s = PPMImage {
     ppmArray :: STArray s (Int,Int) Pixel
    ,ppmWidth :: Int
    ,ppmHeight :: Int
    ,ppmName :: String
    }

data SubImage s = SubImage {
     parent :: PPMImage s
    ,offsetX :: Int
    ,offsetY :: Int
    ,subWidth :: Int
    ,subHeight :: Int
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
    get (SubImage parent x' y' _ _) (x,y) = 
        let (rx, ry) = (x'+x , y'+y)
        in get parent (rx,ry)
    set (SubImage parent x' y' _ _) (x,y) =
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

-- contains information about an image that is useful for mosaic tile matching (only medianColor so far)
data Fingerprint = Fingerprint {
      fpFilename :: String
    , fpMedian :: Pixel
    } deriving (Show)

-- takes a filename and returns its statistics as a string
analyseImage :: String -> IO String
analyseImage filename = do
  rawImage <- BS.readFile filename
  return $ runST $ do
   image <- readPPM (rawImage, filename)
   statStr <- getStatString image
   return statStr

-- takes a ppmImage that is split horizontally and vertically
tileImage :: (PPMImage s) -> Int -> Int -> ST s [SubImage s]
tileImage img partsX partsY = do
  let array = ppmArray img
  let w = ppmWidth img
  let h = ppmHeight img
  let ws = w `div` partsX
  let hs = h `div` partsY
  let xs = [x | x <- [0, ws..w-1]]
  let ys = [y | y <- [0, hs..h-1]]
  let cs = [(x,y) | x <- xs, y <- ys]
  x <- mapM (\(x,y) -> return $ SubImage img x y ws hs) cs
  return x
  
start ::[String] -> IO ()
start ("analyse":arg:_) = do
  dirContent <- getDirectoryContents arg
  let filesWithPPMSuffix = (filter (isSuffixOf ".ppm") dirContent)
  statisticStrings <- mapM analyseImage filesWithPPMSuffix
  writeFile "DB.txt" $ unlines statisticStrings
  print statisticStrings
  
start ("generate":arg:_) = do
  putStrLn "Image will be generated here..."
  db <- readFile "DB.txt"
  let fingerprints = map dbLine2Fingerprint (lines db)
  --print fingerprints
  rawImage <- BS.readFile arg
  let originalImg = runST $ do 
                 image <- readPPM (rawImage, arg)
                 tiles <- tileImage image 2 2
                 subMedians <- mapM medianColorSub tiles
                 return $ subMedians
  print originalImg
  return ()
start _ = putStrLn "Unknown parameter"

-- takes a line from database and returns its information as an image fingerprint
dbLine2Fingerprint :: String -> Fingerprint
dbLine2Fingerprint ln = Fingerprint fn (Pixel (read r) (read g) (read b))
    where (fn:r:g:b:_) = words ln


main :: IO ()
main = start =<< getArgs

-- takes a ppmImage and returns its statistics as a string
getStatString :: (PPMImage s) -> ST s String
getStatString img = do
  medCol <- medianColor img
  return $ printf "%s %s" (ppmName img) (show medCol)

-- Takes a list of strings (representing lines of a ppm file) and turns them
-- into a list of pixels.
str2pix :: [Int] -> [Pixel]
str2pix []         = []
str2pix (r:g:b:xs) =  Pixel r g b : (str2pix xs)

-- Takes a filename and opens it as a ppm image.
readPPM ::  (BS.ByteString, String) -> ST s (PPMImage s)
readPPM (rawImage, fn) = do 
    let content = BS.lines rawImage
        [w,h]   = map (read . BS.unpack) $ BS.words (content !! 1)
        pixel   = readInts $ BS.unwords $ drop 3 content
    xy <- newListArray ((1,1), (w,h)) (str2pix pixel) :: ST s (STArray s (Int,Int) Pixel)
    return PPMImage {
           ppmWidth  = w
         , ppmHeight = h
         , ppmName = fn
         , ppmArray = xy }
  where readInts s =
            case BS.readInt s of
                Nothing       -> []
                Just (v,rest) -> v : readInts (next rest)
        next s = BS.dropWhile isSpace s

-- Calculates the median color of an image.
-- Still a bit clumsy because of getElems
medianColor :: (PPMImage s) -> ST s Pixel
medianColor image = do
    listOfPixel <- getElems $ ppmArray image
    let red =  sum (map pR listOfPixel) `div` ((ppmWidth image) * (ppmHeight image))
    let green =  sum (map pG listOfPixel) `div` ((ppmWidth image) * (ppmHeight image))
    let blue =  sum (map pB listOfPixel) `div` ((ppmWidth image) * (ppmHeight image))
    return $ Pixel red green blue

-- same as medianColor, but for SubImages. 
-- Might be possible to merge these two later on
medianColorSub :: (SubImage s) -> ST s Pixel
medianColorSub image = do
  let coords = [(x,y) | y <- [1..subHeight image], x <- [1..subWidth image]]
  listOfPixel <- mapM (get image) coords
  let red =  sum (map pR listOfPixel) `div` ((subWidth image) * (subHeight image))
  let green =  sum (map pG listOfPixel) `div` ((subWidth image) * (subHeight image))
  let blue =  sum (map pB listOfPixel) `div` ((subWidth image) * (subHeight image))
  return $ Pixel red green blue