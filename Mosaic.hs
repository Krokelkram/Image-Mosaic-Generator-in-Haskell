import Text.Printf


data Pixel = Pixel {
    pr :: Int,
    pg :: Int,
    pb :: Int
    } deriving Show

data Image = Image {
    w :: Int,
    h :: Int,
    pxs :: [Pixel]
    }

main :: IO ()
main = do x <- readPPM "boat.ppm"
          writePPM "copy.ppm" x
          --print $ w x
          return ()

-- takes a list of strings (representing lines of a ppm file) 
-- and turns them into a list of pixels
str2pix :: [String] -> [Pixel]
str2pix [] = []
str2pix (r:g:b:xs) = do let np = Pixel{
                                 pr = (read r),
                                 pg = read g,
                                 pb = read b}
                        np:(str2pix xs)

-- takes a filename and opens it as a ppm image
readPPM :: String -> IO(Image)
readPPM fn = do c <- readFile fn
                let content = lines c
                let res = Image {
                             w = read (words (content !! 1) !! 0) ::Int,
                             h = read (words (content !! 1) !! 1) ::Int,
                             pxs = str2pix $ concat $ map words (drop 3 content)
                             }
                return res

-- takes a single pixel and returns its contents as a string
stringify :: Pixel -> String
stringify px = unwords (show(pr px):show(pg px):show(pb px):[])

-- writes a ppm image under a given filename
writePPM :: String -> Image -> IO()
writePPM fn img = do writeFile fn $ printf "P3\n%d %d\n255\n" (w img) (h img)
                     appendFile fn (unwords $ map stringify $ pxs img)
                     --appendFile fn (unlines $ pix2str $ pxs img)