{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
module Main where
import Text.Printf
import System.Directory
import System.IO
import Data.List
import Data.Char
import Data.Bits
import Control.Parallel
import Control.Monad
import System.Console.CmdArgs
import Graphics.GD.ByteString
import Data.Time
import Types
import Menu


-- the main function takes care of the command line inputs
-- and runs the program in the mode the user has chosen (generate or analyse)
main :: IO ()
main = do
  modus <- cmdArgs "MosaicMaker v0.9" modes
  case modus of
    (Analyse source db) -> do
               putStrLn $ printf "Analyse path: %s" source
               (t,_) <- time $ analyse source db
               putStrLn $ printf "Done in %fs" t
    (Generate f o db' hTiles vTiles) -> do
               putStrLn $ printf "Generate mosaic of: %s" f
               (t, _) <- time $ generate f o db' hTiles vTiles
               putStrLn $ printf "Done in %fs" t
    

-- takes a filename and appends its statistics to a text file
-- dbFolder is the path to database file
analyseImage :: String -> String ->IO ()
analyseImage dbFolder filename = do
  putStrLn $ printf "Analysing %s" filename
  avg <- scaleAndAverage filename dbFolder
  -- append a new line to the file, formatted as 'filename red green blue'
  appendFile (dbFolder ++ "/" ++ "DB.txt") $ printf "%s %s\n" (noSpace filename) (show avg)
  putStrLn "OK"

-- replaces all spaces in a string by underscores
noSpace :: String -> String
noSpace str = map (\x -> if x==' ' then '_' else x) str
  
-- opens the image on the path in first string and saves a smaller copy of it in
-- the folder that is specified with the second string
-- calculating the average color while doing so (which is then returned as a pixel)
scaleAndAverage :: String -> String -> IO Pixel
scaleAndAverage fn folder = 
  withImage (loadJpegFile fn) (\x -> do
                                 resized <- resizeImage 80 60 x
                                 avg <- getJPGavg resized
                                 print avg
                                 saveJpegFile 95 (printf "%s/%s_small.JPG" folder (filenameOnly $ noSpace fn)) resized
                                 return avg)
      where filenameOnly path = removeExtension $ removeFolders path
            removeFolders path = reverse $ takeWhile (/='/') (reverse path)
            removeExtension = takeWhile (/= '.')

-- calculates the average color of an image by sampling every fifth pixel
-- r,g,b values are summed up and divided by the number of taken pixels
-- to get their average, that is then returned as pixel                                          
getJPGavg :: Graphics.GD.ByteString.Image -> IO Pixel
getJPGavg image = do
  (w,h) <- imageSize image
  let coords = [(x,y) | y <- [0,5..h-1], x <- [0,5..w-1]]
  pxs <- mapM (\x -> do 
                 colorC <- getPixel x image
                 let color = fromIntegral colorC :: Int
                 -- GD returns colors as int values, the r g b values are obtained by shifting the number
                 -- and then masking all but the needed bits for that value
                 -- (to get the green value of 0x123456, the number is shifted to 0x001234
                 -- then an 'and' operation with 0x0000FF (255) results in 0x000034, which is what we wanted)
                 return $ Pixel (shiftR color 16 .&. 255) (shiftR color 8 .&. 255) ( color .&. 255)) coords
  let !red   = sum (map pR pxs) `div` length coords
      !green = sum (map pG pxs) `div` length coords
      !blue  = sum (map pB pxs) `div` length coords
      avgPx  = Pixel red green blue
  return avgPx
   

-- analyses a folder by opening the files in it and writing their stats to file
analyse :: String -> String -> IO ()
analyse path dbFolder = do
  putStrLn "Starte Analyse"
  dirContent <- getDirectoryContents path
  -- append the full path to each of the filenames
  let contentWithFullPath = map (\x -> path ++ "/" ++ x) dirContent
  print contentWithFullPath
  createDirectoryIfMissing False dbFolder
  -- create DB.txt or clear it if already exists
  writeFile (dbFolder ++ "/DB.txt") ""
  -- we are only interested in files ending with .jpg or .JPG
  let filesWithPPMSuffix = (filter isJPG contentWithFullPath)
  -- analyse every image
  mapM_ (analyseImage dbFolder) filesWithPPMSuffix
  putStrLn "Done!"
      where isJPG fn = (".JPG" `isSuffixOf` fn) || ".jpg" `isSuffixOf` fn


-- Time a pure function. A should reduce to a single value, instead seq
-- won't work correctly.
pureTime ::(Show a) => [a] -> IO (Double, [a])
pureTime action = do
    d1 <- getCurrentTime
    let a = action
    d2 <- length a `seq` getCurrentTime
    return (read . init $ show (diffUTCTime d2 d1), a)


-- takes the image to 'mosaicify', the path where the result is saved
-- the path to the database and the number of horizontal and vertical tiles
-- to generate the mosaic
generate :: String -> String -> String -> Int -> Int ->IO ()
generate fn outFn dbFolder hTiles vTiles = do
  db <- readFile (dbFolder ++ "/" ++ "DB.txt")
    -- loads the file into a list of fingerprints
  let fingerprints = map dbLine2Fingerprint (lines db)
  img <- loadJpegFile fn
  (w,h) <- imageSize img
    -- calculate the dimensions (width and height) of the tiles
  let tileW  = w `div` hTiles
      tileH  = h `div` vTiles
        -- calculate the upper left corner of every tile
      points = [(x,y) | y <- [0,tileH..h-tileH] , x <- [0,tileW..w-tileH] ]
    -- copy each tile into a new image type and return them in a list
  tiles <- mapM (\p -> do
                   new <- newImage (tileW,tileH)
                   copyRegion p (tileW,tileH) img (0,0) new
                   return new) points
    -- calculate the average color of every tile
  avgs <- mapM getJPGavg tiles
    -- for each tile find the best image that fits its average color
  let bestImageNames = map (findMatch fingerprints) avgs
    -- create a new empty image in the size of the resulting mosaic
  resImage <- newImage (80 * hTiles, 60 * vTiles)
    -- calculate the upper left corners according to the result image
  let offsets   = [(x,y) | y <- [0,60..(60*vTiles)-1], x <- [0,80..(80*hTiles)-1]]
      imgAndOff = zip bestImageNames offsets
    -- insert every image at the position determined by its offset
  mapM_ (insertImage resImage dbFolder) imgAndOff
    -- save the image to file
  saveJpegFile 95 outFn resImage
  
-- takes a target image and a tuple of a filename and offset
-- copies the given image file into the target image, position determined by offset
insertImage :: Graphics.GD.ByteString.Image -> String -> (String, (Int,Int)) -> IO ()
insertImage resImage folder (fn, offset) = do
  isVerbose <- isLoud
  when isVerbose $ putStrLn $ printf "Inserts: %s an %s" fn (show offset)
  -- the inserted image has the name specified in DB.txt, but with '_small' as suffix
  let jpgname = printf "%s/%s_small.JPG" folder (filenameOnly fn)
  withImage (loadJpegFile jpgname) (\x -> copyRegion (0,0) (80,60) x offset resImage)
      where filenameOnly path = removeExtension $ removeFolders path
            removeFolders path = reverse $ takeWhile (/='/') (reverse path)
            removeExtension = takeWhile (/= '.')

-- Time an IO action
time :: IO a -> IO (Double, a)
time action = do
    d1 <- getCurrentTime
    o <- action
    d2 <- getCurrentTime
    return (read . init $ show (diffUTCTime d2 d1), o)

-- takes a list of fingerprints and returns the name of the image that best matches the color of a Pixel
-- this is done by comparing the first two fingerprints in the list, removing the least fitting one
-- then repeating the process until only one fingerprint is left - which must be the best
findMatch :: [Fingerprint] -> Pixel -> String
findMatch fps pix = snd $ minDiff (map (colorDiff pix) fps)
    where minDiff [x] = x
          minDiff (x1:x2:xs) = if fst x1 < fst x2
                               then minDiff $ x1:xs
                               else minDiff $ x2:xs
          minDiff [] = error "Invalid fingerprint"
          
-- returns a tuple of color difference and filename
-- to calculate the difference, the difference between red green and blue channel are calculated
-- by subtracting their absolute value, then adding these three differences
colorDiff :: Pixel -> Fingerprint -> (Int, String)
colorDiff p1 (Fingerprint fn p2) = (abs (abs(pR p1 - pR p2)+abs(pG p1 - pG p2)+abs(pB p1 - pB p2)), fn)

-- takes a line from database and returns its information as an image fingerprint
-- string has to be formatted as 'filename red green blue' separated by whitespaces
dbLine2Fingerprint :: String -> Fingerprint
dbLine2Fingerprint ln = Fingerprint fn (Pixel (read r) (read g) (read b))
    where (fn:r:g:b:_) = words ln