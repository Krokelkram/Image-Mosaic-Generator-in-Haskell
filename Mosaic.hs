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
  putStrLn $ printf "Starte Analyse von: %s" filename
  avg <- saveResizedCopy filename
  appendFile dbname $ printf "%s %s\n" filename (show avg)
  putStrLn $ printf "%s wurde analysiert" filename
  
saveResizedCopy :: String -> IO Pixel
saveResizedCopy fn = do
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
                 return $ Pixel ((shiftR color 16) .&. 255) ((shiftR color 8) .&. 255) ( color .&. 255)) coords
  let !red = sum (map pR pxs) `div` (length coords)
  let !green = sum (map pG pxs) `div` (length coords)
  let !blue = sum (map pB pxs) `div` (length coords)
  let avgPx = Pixel red green blue
  return avgPx
   

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
  
  img <- loadJpegFile fn
  (w,h) <- imageSize img
  let tileW = w `div` 128
  let tileH = h `div` 128
  let points = [(x,y) | y <- [0,tileH..h-1] , x <- [0,tileW..w-1] ]
  tiles <- mapM (\p -> do
                   new <- newImage (tileW,tileH)
                   copyRegion p (tileW,tileH) img (0,0) new
                   return new) points
  avgs <- mapM getJPGavg tiles

  let bestImageNames = map (findMatch fingerprints) avgs
  print bestImageNames

  resImage <- newImage (80 * 128, 60 * 128)
  let offsets   = [(x,y) | y <- [0,60..(60*128)-1], x <- [0,80..(80*128)-1]]
      imgAndOff = zip bestImageNames offsets
  mapM_ (insertImage resImage) imgAndOff
  saveJpegFile 95 "heureka3.jpg" resImage
  print "ENDE"

insertImage :: Graphics.GD.ByteString.Image -> (String, (Int,Int)) -> IO ()
insertImage resImage (fn, offset) = do
  let jpgname = trace (printf "Inserts: %s an %s" fn (show offset)) $ printf "%s_small.JPG" (takeWhile (/= '.') fn)
  withImage (loadJpegFile jpgname) $ do (\x -> do
                                           imgSize <- imageSize x
                                           copyRegion (0,0) (80,60) x offset resImage)

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