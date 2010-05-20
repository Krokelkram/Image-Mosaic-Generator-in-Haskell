module Main where
import Text.Printf
import System.Environment
import System.Directory
import Data.List
import Data.Char
import Control.Parallel
import Control.Monad.ST
import Data.Array.ST
import qualified Data.ByteString.Lazy.Char8 as BS

data Pixel = Pixel {
      pR :: Int
    , pG :: Int
    , pB :: Int
    } deriving Eq

-- Shows contents of a pixel as string.
instance Show Pixel where
    show (Pixel r g b) = printf "%d %d %d" r g b


data Image = Image {
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
  
  --print bla
start ("generate":arg:_) = do
  putStrLn "Image will be generated here..."
  originalImg <- readPPM arg
  db <- readFile "DB.txt"
  let fingerprints = map dbLine2Fingerprint (lines db)

  -- splits the image into segments, each segment is a line for
  -- one of the subimages
  let splitters =  concat $ map (columnsplit vertiCuts) lines
          where vertiCuts = cutPos (imageWidth originalImg) 3
                lines     = linesplit (imageWidth originalImg) (imageData originalImg)
 
  let images =  map (extractSubImage horiCuts) columns
          where horiCuts = cutPos (imageHeight originalImg) 3
                columns  = map (extractColumn 3 splitters) [1..3]
  --writeStats $ concat images
  print (map medianColor (concat images))
  let matches = map (fingerprints `findMatch` ) meds
          where meds = map medianColor (concat images)
  print matches
  print fingerprints
  return ()
start _ = putStrLn "Unknown parameter"

-- takes a median color and a list of fingerprints
-- returns a filename that best fit the median color
findMatch :: [Fingerprint] -> Pixel -> String
findMatch fps med = do
  map (sumPixel.fpMedian) fps
  "bla"

sumPixel :: Pixel -> Int
sumPixel px = pR px + pG px + pB px

dbLine2Fingerprint :: String -> Fingerprint
dbLine2Fingerprint ln = Fingerprint fn (Pixel (read r) (read g) (read b))
    where (fn:r:g:b:_) = words ln

-- splits the pixellist into lines
linesplit :: Int -> [Pixel] -> [[Pixel]]
linesplit _ [] = []
linesplit width pxs = take width pxs : linesplit width (drop width pxs)

-- splits an image line into parts of given length
columnsplit :: [Int] -> [Pixel] -> [[Pixel]]
columnsplit _ [] = []
columnsplit cuts pxs = take (head cuts) pxs : columnsplit (tail cuts) (drop (head cuts) pxs)

-- extracts a single column with given index from the image
extractColumn :: Int -> [[Pixel]] -> Int -> [[Pixel]]
extractColumn _ [] _ = []
extractColumn columns pxs index = pxs !! (index-1) : extractColumn columns (drop columns pxs) index

-- extracts all subimages of a column using the supplied lengths
extractSubImage :: [Int] -> [[Pixel]] -> [Image]
extractSubImage [] _ = []
extractSubImage cuts column = newImage: extractSubImage (tail cuts) (drop (head cuts) column)
    where newImage = Image {
                       imageWidth  = length (head column)
                     , imageHeight = head cuts
                     , imageData   = concat $ take (head cuts) column
                     , imageName   = "part" }


cut :: [Int] -> [Pixel] -> [[Pixel]]
cut [] _ = [] 
cut cutPos pxs = (take (head cutPos) pxs) : (cut (tail cutPos) (drop (head cutPos) pxs) )

-- takes a length (either width or height) and the number of parts we want to cut it into
-- returns the length of each parts (cutPos 10 3 would return [3,3,4])
cutPos :: Int -> Int -> [Int]
cutPos _ 0 = []
cutPos height parts = rowsToTake : cutPos (height - rowsToTake) (parts - 1)
    where rowsToTake = (height `div` parts)


main :: IO ()
main = start =<< getArgs


-- Writes statistics about all images in a list to file.
writeStats :: [Image] -> IO ()
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
readPPM :: String -> IO Image
readPPM fn = do 
    c <- BS.readFile fn
    let content = BS.lines c
        [w,h]   = map (read . BS.unpack) $ BS.words (content !! 1)
        pixel   = readInts $ BS.unwords $ drop 3 content
    return $ Image {
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
writePPM :: String -> Image -> IO()
writePPM fn (Image w h dat _) = do 
    writeFile fn $ printf "P3\n%d %d\n255\n" w h
    appendFile fn (unwords $ map show dat)


-- Calculates the median color of an image.
medianColor :: Image -> Pixel
medianColor (Image width height pixel _) = 
    Pixel (average pR) (average pG) (average pB)
  where average comp = sum (map comp pixel) `div` (width * height)

