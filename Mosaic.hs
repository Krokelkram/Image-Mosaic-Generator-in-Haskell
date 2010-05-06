module Main where
import Text.Printf
import System.Environment
import System.Directory
import Data.List
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


main :: IO ()
main = do 
    dirContent <- getDirectoryContents =<< head `fmap` getArgs 
    images     <- mapM readPPM (filter (isSuffixOf ".ppm") dirContent)
    writeStats images


-- Writes statistics about all images in a list to file.
writeStats :: [Image] -> IO ()
writeStats files = 
    mapM_ forEachFile files
  where forEachFile img = do
            let dat = printf "%s %s\n" (imageName img) (show (medianColor img))
            appendFile "DB.txt" dat
          

-- Takes a list of strings (representing lines of a ppm file) and turns them
-- into a list of pixels.
str2pix :: [String] -> [Pixel]
str2pix []         = []
str2pix (r:g:b:xs) =  
    let val = Pixel{ pR = (r),pG = g, pB = b} 
    in val : (str2pix xs)


-- Takes a filename and opens it as a ppm image.
readPPM :: String -> IO Image
readPPM fn = do 
    c <- readFile fn
    let content = lines c
        [w,h]   = map read $ words (content !! 1)
        pixel   = map read $ words $ unlines $ drop 3 content
    return $ Image {
           imageWidth  = w
         , imageHeight = h
         , imageData   = str2pix pixel
         , imageName   = fn }


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

