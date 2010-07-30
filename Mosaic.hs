{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
module Main where
import Text.Printf
import System.Environment
import System.Directory
import System.IO
import Data.List
import Data.Char
import Data.Bits
import Control.Parallel
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Debug.Trace
import Graphics.GD.ByteString
import qualified Data.ByteString.Char8 as BS

import qualified Data.Foldable as DF
import Control.Parallel.Strategies (rnf)

import Data.Time

import Types

main :: IO ()
main = do
  args <- getArgs
  case head args of
    "analyse" -> analyse $ tail args
    "generate" -> generate' $ tail args
    
                           

-- takes a filename and appends its statistics to a text file
analyseImage :: String -> String ->IO ()
analyseImage dbname filename = do
  --rawImage <- BS.readFile filename
  putStrLn $ printf "Starte Analyse von: %s" filename
  --appendFile dbname $ runST $ do
  -- image <- trace "Einlesen" $ readPPM (rawImage, filename)
  -- statStr <- trace "medString berechnen" $ getStatString image
  -- return $ trace statStr $ statStr

  avg <- saveResizedCopy filename
  appendFile dbname $ printf "%s %s\n" filename (show avg)
  putStrLn $ printf "%s wurde analysiert" filename
  

saveResizedCopy :: String -> IO Pixel
saveResizedCopy fn = do
  --let jpgname = trace (printf "Resize to mini: %s" fn) $ printf "%s.JPG" (takeWhile (/= '.') fn)
  let jpgname = fn
  avg <- withImage (loadJpegFile jpgname) $ do (\x -> do
                                                  resized <- resizeImage 80 60 x
                                                  avg <- getJPGavg resized
                                                  print avg
                                                  saveJpegFile 95 (printf "%s_small.JPG" (takeWhile (/= '.') fn)) resized
                                                  return avg)
  return avg
                                          
  

getJPGavg :: Graphics.GD.ByteString.Image -> IO Pixel
getJPGavg image = do
  (w,h) <- imageSize image
  let coords = [(x,y) | y <- [0,5..h-1], x <- [0,5..w-1]]
  pxs <- mapM (\x -> do 
                 colorC <- getPixel x image
                 let color = fromIntegral colorC :: Int
                 --trace (printf"%d %d %d (%d)" ((shiftR color 16) .&. 255) ((shiftR color 8) .&. 255) ( color .&. 255)  color) $ return $fromIntegral color) coords
                 return $ Pixel ((shiftR color 16) .&. 255) ((shiftR color 8) .&. 255) ( color .&. 255)) coords
  --let avg = sum pxs `div` length coords
  let !red = sum (map pR pxs) `div` (length coords)
  let !green = sum (map pG pxs) `div` (length coords)
  let !blue = sum (map pB pxs) `div` (length coords)
  let avgPx = Pixel red green blue
  return avgPx
   

-- takes a ppmImage that is split horizontally and vertically
tileImage :: (PPMImage s) -> Int -> Int -> ST s [SubImage s]
tileImage img partsX partsY = return =<< mapM (\(x,y) -> return $! SubImage img x y ws hs) cs
  where w = ppmWidth img
        h = ppmHeight img
        ws = w `div` partsX
        hs = h `div` partsY
        cs = [(x,y) | y <- getCutList partsY h, x <- getCutList partsX w]

-- takes a number of parts and a length that is then cut into that many parts
-- returns a list of cutpositions
-- example: to cut '14' into 4 parts of almost the same size, we can cut it at [0,4,8,11]
getCutList :: Int -> Int -> [Int]
getCutList 1 length = [0]
getCutList parts length = getCutList (parts-1) (length-section) ++ [length-section]
    where section = floor $ (fromIntegral length) / (fromIntegral parts)

-- rescales an image to the given size
rescale :: Int -> Int -> (BS.ByteString, String) -> ST s (PPMImage s)
rescale w h (rawPixel, fname) = do
  img <- readPPM (rawPixel, fname)
  tiles <- tileImage img w h
  pixels <- mapM medianColorSub tiles
  !pxArr <- newListArray ((1,1), (w,h)) pixels :: ST s (STArray s (Int,Int) Pixel)
  let finalImage = PPMImage pxArr w h "final"
  return finalImage

-- pastes a small image into a bigger image at the given position
pasteImage :: PPMImage s -> (BS.ByteString, (Int,Int)) -> ST s ()
pasteImage finalImg (tileImg', (offX, offY)) = do
  tileImg <- rescale 80 80 (tileImg', "nix")
  trace "P" $ forM_ [1..ppmWidth tileImg] $ \x -> do
    forM_ [1..ppmHeight tileImg] $ \y -> do
      pixel <- get tileImg (x,y)
      set finalImg (x+offX,y+offY) pixel
  return ()

pasteImage' :: PPMImage s -> [(BS.ByteString, (Int,Int))] -> ST s ()
pasteImage' _ [] = return ()
pasteImage' finalImg ((tileImg', (offX, offY)):xs) = do
  tileImg <- rescale 80 80 (tileImg', "nix")
  forM_ [1..ppmWidth tileImg] $ \x -> do
    forM_ [1..ppmHeight tileImg] $ \y -> do
      !pixel <- get tileImg (x,y)
      set finalImg (x+offX,y+offY) pixel
  pasteImage' finalImg xs

-- merges all given images into one big image
-- needs the size of the small images and the number of rows and columns
merge :: [BS.ByteString] -> (Int,Int) -> (Int,Int) -> ST s (PPMImage s)
merge tileImages (tileW, tileH) (partsX,partsY) = do
  pxs <- newArray ((1,1),(partsY * tileH, partsX * tileW)) (Pixel 0 0 0):: ST s (STArray s (Int,Int) Pixel)
  let finalImage = PPMImage pxs (partsX * tileW) (partsY * tileH) "bla"
  let mergeOffset = [(x,y) | y <- [0,tileH..((partsY * tileH)-1)], x <- [0, tileW..((partsX * tileW)-1)]]
  let tileOffsetPairs = zip tileImages mergeOffset
  --mapM_ (pasteImage finalImage) tileOffsetPairs
  pasteImage' finalImage tileOffsetPairs
  return $ finalImage

mergeNew :: [[Pixel]] -> (Int,Int) -> (Int,Int) -> ST s (PPMImage s)
mergeNew tiles (tW,tH) (pX,pY) = do
  pxs <- newArray ((1,1),(pY * tH, pX * tW)) (Pixel 90 90 90):: ST s (STArray s (Int,Int) Pixel)
  let finalImage = PPMImage pxs (pX * tW) (pY * tH) "bla"
  let mergeOffset = [(x,y) | y <- [0,tH..((pY * tH)-1)], x <- [0, tW..((pX * tW)-1)]]
  let tileOffsetPairs = zip tiles mergeOffset
  pasteImageNew finalImage tileOffsetPairs (tW,tH)
  return $ finalImage

pasteImageNew :: PPMImage s -> [([Pixel], (Int,Int))] -> (Int,Int) -> ST s ()
pasteImageNew _ [] _ = return ()
pasteImageNew finalImg ((tileImg, (offX, offY)):xs) (w,h) = do
  --tileImg <- rescale 80 80 (tileImg', "nix")
  forM_ [1..w] $ \x -> do
    forM_ [1..h] $ \y -> do
      -- !pixel <- get tileImg (x,y)
      set finalImg (x+offX,y+offY) (tileImg !! (((y-1)* w) + (x-1)))
  pasteImageNew finalImg xs (w,h)

-- analyses a folder by opening the files in it and writing their stats to file
analyse :: [String] -> IO ()
analyse (path:xs) = do
  putStrLn "Starte Analyse"
  dirContent <- getDirectoryContents path
  writeFile "DB.txt" ""
  let filesWithPPMSuffix = (filter ((isSuffixOf ".JPG")) dirContent)
  mapM (analyseImage "DB.txt") filesWithPPMSuffix
  print "Fertig"


-- Time a pure function. A should reduce to a single value, instead seq
-- won't work correctly.
pureTime ::(Show a) => [a] -> IO (Double, [a])
pureTime action = do
    d1 <- getCurrentTime
    let a = action
    -- print a
    d2 <- (length a) `seq` getCurrentTime
    return (read . init $ show (diffUTCTime d2 d1), a)


generate' :: [String] ->IO ()
generate' (fn:_) = do
  putStrLn "Bild wird generiert..."
  db <- readFile "DB.txt"
  let fingerprints = map dbLine2Fingerprint (lines db)
  --rawImage <- BS.readFile fn
  --(t1,originalImg) <- pureTime $ runST $ do
  --               image <- readPPM (rawImage, fn)
  --               tiles <- tileImage image 64 64
  --               subMedians <- mapM medianColorSub tiles
  --               return $ subMedians
  --print t1




  avg <- withImage (loadJpegFile fn) $ do (\x -> do
                                             (w,h) <- imageSize fn
                                             let tileW = w `div` 64
                                             let tileH = h `div` 64
                                             let points = [(x,y) | y <- [0,tileH..h-1] , x <- [0,(w/64)..w-1] ]
                                             mapM (\x -> do
                                                     new <- newImage ()
                                                     copyRegion x ) points
                                             --resized <- resizeImage 80 60 x
                                             avg <- getJPGavg resized
                                             print avg
                                             saveJpegFile 95 (printf "%s_small.JPG" (takeWhile (/= '.') fn)) resized
                                             return avg)




  let bestImageNames = map (findMatch fingerprints) originalImg
  print bestImageNames

  resImage <- newImage (80 * 64, 60 * 64)
  let offsets   = [(x,y) | y <- [0,60..(60*64)-1], x <- [0,80..(80*64)-1]]
      imgAndOff = zip bestImageNames offsets
  mapM_ (insertImage resImage) imgAndOff
  saveJpegFile 95 "heureka3.jpg" resImage
  
 -- rescImgPxs <- openAndRescale bestImageNames
 -- putStr $ show $ length rescImgPxs
 -- let pxs = runST $ do
 --                    res <- mergeNew rescImgPxs (80,80) (5,5)
 --                    elems <- getElems $ ppmArray res
 --                    return elems
 -- (length rescImgPxs) `seq` writePPM (pxs) (400,400) "badtest.ppm"
  print "ENDE"

insertImage :: Graphics.GD.ByteString.Image -> (String, (Int,Int)) -> IO ()
insertImage resImage (fn, offset) = do
  let jpgname = trace (printf "Inserts: %s an %s" fn (show offset)) $ printf "%s_small.JPG" (takeWhile (/= '.') fn)
  withImage (loadJpegFile jpgname) $ do (\x -> do
                                           imgSize <- imageSize x
                                           copyRegion (0,0) (80,60) x offset resImage)
                                           --copyRegionScaled (0,0) imgSize x offset (80,80) resImage)

buildMosaic :: [[Pixel]] -> Int -> Int -> [[Pixel]]
buildMosaic [] _ _ = []
buildMosaic imgs rowLength tilesPerRow = (concat $ buildTileRow (take tilesPerRow imgs) rowLength) : buildMosaic (drop rowLength imgs) rowLength tilesPerRow

buildTileRow :: [[Pixel]] -> Int -> [[Pixel]]
buildTileRow [] _ = []
buildTileRow imgs rowLength = (concat (map (take rowLength) imgs)) : buildTileRow ( map (drop rowLength) imgs) rowLength

openAndRescale :: [String] -> IO [[Pixel]]
openAndRescale [] = return []
openAndRescale (fn:xs) = do
  !file <- trace "read" $ BS.readFile fn
  putStr "*"                         
  (t1,res) <- pureTime $ trace "ST" $ runST $ do
               !img <- readPPM (file,fn)
               !tiles <- tileImage img 80 80
               !p <- mapM medianColorSub tiles
               return $ (length p) `seq` p
  putStr $ printf "Zeit: %f" t1
  xs' <- (length res) `seq` openAndRescale xs
  
  return $ res : xs'


-- generates a mosaic using the given filename as reference
generate :: [String] -> IO ()
generate (fn:xs) = do
  putStrLn "Image will be generated here..."
  db <- readFile "DB.txt"
  let fingerprints = map dbLine2Fingerprint (lines db)
  rawImage <- BS.readFile fn
  (t1,originalImg) <- pureTime $ runST $ do
                 image <- readPPM (rawImage, fn)
                 tiles <- tileImage image 10 10
                 subMedians <- mapM medianColorSub tiles
                 return $ subMedians
  print t1
  
  let bestImageNames = map (findMatch fingerprints) originalImg
  rawTileFiles <- mapM (\x -> do
                          putStr "."
                          BS.readFile x) bestImageNames
  putStrLn "Huhu"
  let finalImg = runST $ do
                     finalImg <- merge rawTileFiles (80,80) (10,10)
                     bla <- getElems $ ppmArray finalImg
                     return bla
  print bestImageNames
  writePPM finalImg (800,800) "final.ppm"

-- Time an IO action
time :: IO a -> IO (Double, a)
time action = do
    d1 <- getCurrentTime
    o <- action
    d2 <- getCurrentTime
    return (read . init $ show (diffUTCTime d2 d1), o)

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
  medCol <- trace "call: medianColor" $ medianColor img
  return $ printf "%s %s\n" (ppmName img) (show medCol)

-- Takes a list of strings (representing lines of a ppm file) and turns them
-- into a list of pixels.
str2pix :: [Int] -> [Pixel]
str2pix [] = []
str2pix (r:g:b:xs) = Pixel r g b : (str2pix xs)

writePPM :: [Pixel] -> (Int,Int) -> String -> IO ()
writePPM pxs (w,h) fname = do
  putStrLn "Jetzt wird geschrieben"
  writeFile fname $ printf "P3\n%d %d\n255\n" w h
  trace "Writing..." $ appendFile fname (unwords $ map show (pxs))


-- Takes a filename and opens it as a ppm image.
readPPM :: (BS.ByteString, String) -> ST s (PPMImage s)
readPPM (rawImage, fn) = do
    let content = BS.lines rawImage
        [w,h] = map (read . BS.unpack) $ BS.words (content !! 1)
        pixel = readInts $ BS.unwords $ drop 3 content
    xy <- newListArray ((1,1), (h,w)) (str2pix pixel) :: ST s (STArray s (Int,Int) Pixel)
    (length pixel) `seq` return PPMImage {
           ppmWidth = w
         , ppmHeight = h
         , ppmName = fn
         , ppmArray = xy }
  where readInts s =
            case BS.readInt s of
                Nothing -> []
                Just (v,rest) -> v : readInts (next rest)
        next s = BS.dropWhile isSpace s

-- Calculates the median color of an image.
-- Still a bit clumsy because of getElems
medianColor :: (PPMImage s) -> ST s Pixel
medianColor image = do
    !listOfPixel <- trace "getElems" $ getElems $ ppmArray image
    let !red = trace "RedSum" $ sum (map pR listOfPixel) `div` ((ppmWidth image) * (ppmHeight image))
    let !green = sum (map pG listOfPixel) `div` ((ppmWidth image) * (ppmHeight image))
    let !blue = sum (map pB listOfPixel) `div` ((ppmWidth image) * (ppmHeight image))
    let !resPixel = Pixel red green blue
    return resPixel


-- same as medianColor, but for SubImages.
-- Might be possible to merge these two later on
medianColorSub :: (SubImage s) -> ST s Pixel
medianColorSub image = do
  let !coords = [(x,y) | y <- [1,10..subHeight image], x <- [1,10..subWidth image]]
  !listOfPixel <- mapM (get image) coords
  let !red = sum (map pR listOfPixel) `div` (length coords)
  let !green = sum (map pG listOfPixel) `div` (length coords)
  let !blue = sum (map pB listOfPixel) `div` (length coords)
  let !resPixel = Pixel red green blue
  return resPixel