import Text.Printf
import System.Environment
import System.Directory
import Data.List
import Control.Parallel
import qualified Data.ByteString.Lazy.Char8 as BS

data Pixel = Pixel {
    pR :: Int,
    pG :: Int,
    pB :: Int
    }

data Image = Image {
    w :: Int,
    h :: Int,
    pxs :: [Pixel],
    iname :: String
    }

-- writes statistics about all images in a list to file
writeStats :: [Image] -> IO ()
writeStats [] = return ()
writeStats (x:xs) = do
  appendFile "DB.txt" $ printf "%s %s\n" (iname x) ((Main.show (medianColor x))::String)
  writeStats xs

main :: IO ()
main = do args <- getArgs
          writeFile "DB.txt" ""
          dirContent <- getDirectoryContents (args !! 0)
          let ppmFileNames = filter (isSuffixOf ".ppm") dirContent
          print ppmFileNames
          images <- mapM readPPM ppmFileNames
          writeStats images
         -- x <- readPPM "minippm_0.ppm"
         -- putStrLn $ Main.show $ medianColor x
          
          return ()

-- takes a list of strings (representing lines of a ppm file) 
-- and turns them into a list of pixels
-- str2pix :: [String] -> [Pixel]
str2pix [] = []
str2pix (r:g:b:xs) =  
    let val = Pixel{ pR = (r),pG = g, pB = b} 
    in val : (str2pix xs)



-- takes a filename and opens it as a ppm image
readPPM :: String -> IO(Image)
readPPM fn = do c <- BS.readFile fn
                let content = BS.lines c
                    pixel   = map (read. BS.unpack) $ BS.words $ BS.unlines $ drop 3 content :: [Int]
                return $ Image {
                             w = read $ BS.unpack $ (BS.words (content !! 1) !! 0) ::Int,
                             h = read $ BS.unpack $ (BS.words (content !! 1) !! 1) ::Int,
                             pxs = str2pix pixel, -- $ map ss $ concat $ map words (drop 3 content),
                             iname = fn
                             }

-- gives contents of a pixel as string
instance Show Pixel where
show (Pixel r g b) = printf "%d %d %d" r g b

-- writes a ppm image under a given filename
writePPM :: String -> Image -> IO()
writePPM fn img = do writeFile fn $ printf "P3\n%d %d\n255\n" (w img) (h img)
                     appendFile fn (unwords $ map Main.show $ pxs img)

-- calculates the median color of an image
medianColor :: Image -> Pixel
medianColor img = Pixel {
                    pR = (sum $ map pR $ pxs img ) `div` (length $ pxs img),
                    pG = (sum $ map pG $ pxs img ) `div` (length $ pxs img),
                    pB = (sum $ map pB $ pxs img ) `div` (length $ pxs img)}
