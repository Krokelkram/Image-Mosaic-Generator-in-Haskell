module Main where
import Text.Printf
import System.Environment
import System.Directory
import Data.List
import Data.Char
import Control.Parallel
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


start ::[String] -> IO ()
start ("analyse":arg:_) = do
  dirContent <- getDirectoryContents arg
  images     <- mapM readPPM (filter (isSuffixOf ".ppm") dirContent)
  writeStats images
start ("generate":arg:_) = do
  putStrLn "Image will be generated here..."
  originalImg <- readPPM arg
  let horiCuts = map (*imageWidth originalImg) $ cutPos (imageHeight originalImg) 3
  let vertiCuts = cutPos (imageWidth originalImg) 3
  --let subimages = map ( `take` (imageData originalImg)) horiCuts
  --print $ imageData originalImg
  --print horiCuts
  let scheiben =  cut horiCuts (imageData originalImg)
  --print scheiben
  --let bla = (cutVert vertiCuts 1)
  --print $ concat $ map (reverse.cutVert vertiCuts 2) scheiben
  --print $ cutVert vertiCuts 2 (scheiben !! 0)

  let lines = linesplit (imageWidth originalImg) (imageData originalImg)
  let splitters =  concat $ map (columnsplit vertiCuts) lines
  --print splitters
  let columns = map (extractColumn 3 splitters) [1..3]
  print columns
  
  return ()
start _ = putStrLn "Unknown parameter"

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
--extractSubImage :: [Int] -> 


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

