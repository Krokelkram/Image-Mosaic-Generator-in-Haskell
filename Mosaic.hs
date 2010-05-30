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

import Types

main :: IO ()
main = do
  args <- getArgs
  case head args of
    "analyse" -> analyse $ tail args
    "generate" -> generate $ tail args

-- takes a filename and returns its statistics as a string
analyseImage :: String -> String ->IO ()
analyseImage dbname filename= do
  rawImage <- BS.readFile filename
  appendFile dbname $ runST $ do
   image <- readPPM (rawImage, filename)
   statStr <- getStatString image
   return statStr
   

-- takes a ppmImage that is split horizontally and vertically
tileImage :: (PPMImage s) -> Int -> Int -> ST s [SubImage s]
tileImage img partsX partsY = return =<< mapM (\(x,y) -> return $ SubImage img x y ws hs) cs
  where w     = ppmWidth img
        h     = ppmHeight img
        ws    = w `div` partsX
        hs    = h `div` partsY
        cs    = [(x,y) | x <- [0, ws..w-1], y <- [0, hs..h-1]]
  
analyse :: [String] -> IO ()
analyse (path:xs) = do
  dirContent <- getDirectoryContents path
  writeFile "DB.txt" ""
  let filesWithPPMSuffix = (filter (isSuffixOf ".ppm") dirContent)
  mapM (analyseImage "DB.txt") filesWithPPMSuffix
  print "Fertig"

generate :: [String] -> IO ()
generate (fn:xs) = do
  putStrLn "Image will be generated here..."
  db <- readFile "DB.txt"
  let fingerprints = map dbLine2Fingerprint (lines db)
  rawImage <- BS.readFile fn
  let originalImg = runST $ do 
                 image <- readPPM (rawImage, fn)
                 tiles <- tileImage image 2 2
                 subMedians <- mapM medianColorSub tiles
                 return $ subMedians
  print originalImg
  print $ map (findMatch fingerprints) originalImg

-- takes a list of fingerprints and returns the name of the image that best matches the color of a Pixel
findMatch :: [Fingerprint] -> Pixel -> String
findMatch fps pix = snd $ minDiff (map (colorDiff pix) fps)
    where minDiff [x] = x
          minDiff (x1:x2:xs) = if (fst x1) < (fst x2)
                               then minDiff $ x1:xs
                               else minDiff $ x2:xs
          

-- returns a tuple of color difference and filename
colorDiff :: Pixel -> Fingerprint -> (Int, String)
colorDiff p1 (Fingerprint fn p2) = (abs ((pR p1 - pR p2)+(pG p1 - pG p2)+(pB p1 - pB p2)), fn)

-- takes a line from database and returns its information as an image fingerprint
dbLine2Fingerprint :: String -> Fingerprint
dbLine2Fingerprint ln = Fingerprint fn (Pixel (read r) (read g) (read b))
    where (fn:r:g:b:_) = words ln

-- takes a ppmImage and returns its statistics as a string
getStatString :: (PPMImage s) -> ST s String
getStatString img = do
  medCol <- medianColor img
  return $ printf "%s %s\n" (ppmName img) (show medCol)

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