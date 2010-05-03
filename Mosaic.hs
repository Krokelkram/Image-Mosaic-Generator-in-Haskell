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

str2pix :: [String] -> [Pixel]
str2pix [] = []
str2pix (r:g:b:xs) = do let np = Pixel{
                                 pr = (read r),
                                 pg = read g,
                                 pb = read b}
                        np:(str2pix xs)

readPPM :: String -> IO(Image)
readPPM fn = do c <- readFile fn
                let content = lines c
                let dummy2 = Image {
                             w = read (words (content !! 1) !! 0) ::Int,
                             h = read (words (content !! 1) !! 1) ::Int,
                             pxs = str2pix $ concat $ map words (drop 3 content)
                             }
                return dummy2

stringify :: Pixel -> String
stringify px = unwords (show(pr px):show(pg px):show(pb px):[])

writePPM :: String -> Image -> IO()
writePPM fn img = do writeFile fn $ printf "P3\n%d %d\n255\n" (w img) (h img)
                     appendFile fn (unwords $ map stringify $ pxs img)
                     --appendFile fn (unlines $ pix2str $ pxs img)