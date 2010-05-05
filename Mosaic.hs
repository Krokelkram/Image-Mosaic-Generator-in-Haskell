import Text.Printf
import System.Environment
import System.Directory
import Data.List


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

writeStats :: [Image] -> IO ()
writeStats imgs = do let medColors = map medianColor imgs
                     writeFile "DB.txt" "TODO"

main :: IO ()
main = do args <- getArgs
          dirContent <- getDirectoryContents (args !! 0)
          let ppmFileNames = filter (isSuffixOf ".ppm") dirContent
          print ppmFileNames
          images <- mapM readPPM ppmFileNames
          --let medColors = map medianColor images
          --let medStrs = map Main.show medColors :: [String]
          --print medStrs
          
          x <- readPPM "boat.ppm"
          putStrLn $ Main.show $ medianColor x
          return ()

-- takes a list of strings (representing lines of a ppm file) 
-- and turns them into a list of pixels
str2pix :: [String] -> [Pixel]
str2pix [] = []
str2pix (r:g:b:xs) =  Pixel{ pR = (read r),pG = read g, pB = read b} : (str2pix xs)

-- takes a filename and opens it as a ppm image
readPPM :: String -> IO(Image)
readPPM fn = do c <- readFile fn
                print "Beginne Lesen"
                let content = lines c
                return $ Image {
                             w = read (words (content !! 1) !! 0) ::Int,
                             h = read (words (content !! 1) !! 1) ::Int,
                             pxs = str2pix $ concat $ map words (drop 3 content),
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
