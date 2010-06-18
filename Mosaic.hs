{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
module Main where
import Text.Printf
import System.Environment
import System.Directory
import Data.List
import Data.Char
import Control.Parallel
import Control.Monad
import Control.Monad.ST.Lazy
import Data.Array.ST
import Debug.Trace
import qualified Data.ByteString.Lazy.Char8 as BS

import Data.Time
--import Data.Time.Clock.Posix

import Types

main :: IO ()
main = do
  args <- getArgs
  case head args of
    "analyse" -> analyse $ tail args
    "generate" -> generate $ tail args
    "debug" -> print =<< do
                   rawImage <- BS.readFile "test4x6.ppm"
                   let pxs = runST $ do 
                               i <- readPPM (rawImage, "test6x6.ppm")
                               merged <- merge [i,i,i,i] (4,6) (2,2)
                               --scaled <- rescale 100 100 i 
                               px <- getElems $ ppmArray merged
                               return px
                   writePPM pxs (4,6) "blatest.ppm"
                   return "done"
    "debug2" -> print $ runST $ do
                   xy <- newListArray ((1,1),(5,4)) [1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5] :: ST s (STArray s (Int,Int) Int)
                   --res <- getElems xy
                   res <- readArray xy (1,4)
                   return res
                           

-- takes a filename and appends its statistics to a text file
analyseImage :: String -> String ->IO ()
analyseImage dbname filename = do
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
        cs    = [(x,y) | y <- getCutList partsY h, x <- getCutList partsX w]

-- takes a number of parts and a length that is then cut into that many parts
-- returns a list of cutpositions
-- example: to cut '14' into 4 parts of almost the same size, we can cut it at [0,4,8,11]
getCutList :: Int -> Int -> [Int]
getCutList 1 length = [0]
getCutList parts length =  getCutList (parts-1) (length-section) ++ [length-section]
    where section = floor $ (fromIntegral length) / (fromIntegral parts)

-- rescales an image to the given size
rescale :: Int -> Int -> (BS.ByteString, String) -> ST s (PPMImage s)
rescale w h (rawPixel, fname) = do
  !img <- readPPM (rawPixel, fname)
  !tiles <- tileImage img w h 
  !pixels <- mapM medianColorSub tiles
  !pxArr <- newListArray ((1,1), (w,h)) pixels :: ST s (STArray s (Int,Int) Pixel)
  let finalImage = PPMImage pxArr w h "final"
  return finalImage

-- pastes a small image into a bigger image at the given position
pasteImage :: PPMImage s -> (PPMImage s, (Int,Int)) -> ST s ()
pasteImage finalImg (tileImg, (offX, offY)) = do
  forM_ [1..ppmWidth tileImg] $ \x -> do
    forM_ [1..ppmHeight tileImg] $ \y -> do
      pixel <- get tileImg (x,y)
      set finalImg (x+offX,y+offY) pixel
  return ()

-- merges all given images into one big image
-- needs the size of the small images and the number of rows and columns
merge :: [PPMImage s] -> (Int,Int) -> (Int,Int) -> ST s (PPMImage s)
merge tileImages (tileW, tileH) (partsX,partsY) = do
  pxs <- newArray ((1,1),(partsY * tileH, partsX * tileW)) (Pixel 0 0 0):: ST s (STArray s (Int,Int) Pixel)
  let finalImage = PPMImage pxs (partsX * tileW) (partsY * tileH) "bla"
  let mergeOffset = [(x,y) | y <- [0,tileH..((partsY * tileH)-1)], x <- [0, tileW..((partsX * tileW)-1)]]
  let tileOffsetPairs = zip tileImages mergeOffset
  mapM_ (pasteImage finalImage) tileOffsetPairs 
  return $ finalImage

-- analyses a folder by opening the files in it and writing their stats to file
analyse :: [String] -> IO ()
analyse (path:xs) = do
  dirContent <- getDirectoryContents path
  writeFile "DB.txt" ""
  let filesWithPPMSuffix = (filter (isSuffixOf ".ppm") dirContent)
  mapM (analyseImage "DB.txt") filesWithPPMSuffix
  print "Fertig"


-- Time a pure function. A should reduce to a single value, instead seq
-- won't work correctly.
pureTime ::(Show a) => a -> IO (Double, a)
pureTime action = do
    d1 <- getCurrentTime
    let a = action
    print a
    d2 <- a `seq` getCurrentTime
    return (read . init $ show (diffUTCTime d2 d1), a)

-- generates a mosaic using the given filename as reference
generate :: [String] -> IO ()
generate (fn:xs) = do
  putStrLn "Image will be generated here..."
  db <- readFile "DB.txt"
  let fingerprints = map dbLine2Fingerprint (lines db)
  rawImage <- BS.readFile fn
  (t1,originalImg) <- pureTime $ runST $ do 
                 image <- readPPM (rawImage, fn)
                 tiles <- tileImage image 100 100
                 subMedians <- mapM medianColorSub tiles
                 return $ subMedians
  --print originalImg
  print t1
  
  let bestImageNames = map (findMatch fingerprints) originalImg
  rawTileFiles <- mapM BS.readFile bestImageNames
  
  let finalImg = runST $ do
                     scaledTileFiles <- mapM (rescale 80 80) (zip rawTileFiles bestImageNames)
                     !finalImg <- merge scaledTileFiles (80,80) (10,10) 
                     bla <- getElems $ ppmArray finalImg
                     return bla
  print bestImageNames
  --print finalImg
  writePPM finalImg (800,800) "final.ppm" 

-- takes a list of fingerprints and returns the name of the image that best matches the color of a Pixel
findMatch :: [Fingerprint] -> Pixel -> String
findMatch fps pix = snd $ minDiff (map (colorDiff pix) fps)
    where minDiff [x] = x
          minDiff (x1:x2:xs) = if (fst x1) < (fst x2)
                               then minDiff $ x1:xs
                               else minDiff $ x2:xs
          

-- returns a tuple of color difference and filename
colorDiff :: Pixel -> Fingerprint -> (Int, String)
colorDiff p1 (Fingerprint fn p2) = (abs (abs(pR p1 - pR p2)+abs(pG p1 - pG p2)+abs(pB p1 - pB p2)), fn)

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

writePPM :: [Pixel] -> (Int,Int) -> String -> IO ()
writePPM pxs (w,h) fname = do
  writeFile fname $ printf "P3\n%d %d\n255\n" w h
  appendFile fname (unwords $ map show (pxs))

-- Takes a filename and opens it as a ppm image.
readPPM ::  (BS.ByteString, String) -> ST s (PPMImage s)
readPPM (rawImage, fn) = do 
    let content = BS.lines rawImage
        [w,h]   = map (read . BS.unpack) $ BS.words (content !! 1)
        pixel   = readInts $ BS.unwords $ drop 3 content
    !xy <- newListArray ((1,1), (h,w)) (str2pix pixel) :: ST s (STArray s (Int,Int) Pixel)
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
  let !coords = [(x,y) | y <- [1,10..subHeight image], x <- [1,10..subWidth image]]
  !listOfPixel <- mapM (get image) coords
  let !red =  sum (map pR listOfPixel) `div` (length coords)
  let !green =  sum (map pG listOfPixel) `div` (length coords)
  let !blue =  sum (map pB listOfPixel) `div` (length coords)
  return $ Pixel red green blue