import System.Environment
import Text.Printf
import Data.Complex
import Data.List
import Char

-- transforms image coords to mandelbrot coords
transform ::  Int -> Int -> (Int,Int) -> (Float,Float)
transform w h (x, y) = (x',y')
                       where x' = (( 3.0 / fromIntegral w) * fromIntegral x) - 2.0
                             y' = (( 2.0 / fromIntegral h) * fromIntegral y) - 1.0

-- checks a list of iterations for convergence
converge :: (RealFloat a) => Bool -> [Complex a] -> Bool
converge False _ = False
converge True [] = True
converge True (x:xs) = converge (magnitude(x)<2) xs

-- checks if a point is drawn or not
pointCheck :: (Float, Float) -> Int
pointCheck (x,y) = do let a = (x :+ y)
                      let iters = iterate (\z -> z^2 + a) a 
                      fromEnum $ converge True ((take 100)(iters))

-- makes a list of integers to a string with whitespaces between entries
stringify :: [Int] -> String
stringify list = intercalate " " $ map show list
                 
-- writes the image to a ppm file
write :: PPM -> IO()
write p = do let headStr = printf "P1\n# %s\n%d %d\n" (pName p) (pWidth p) (pHeight p) :: String
             writeFile (pName p) headStr
             appendFile (pName p) (stringify (pImage p))
             return ()

-- holds information about the image
data PPM = PPM {
    pWidth :: Int,
    pHeight :: Int,
    pName :: String,
    pImage :: [Int]
    }

main :: IO ()
main = do args <- getArgs
          let width = read(args !! 0) :: Int
          let height = read(args !! 1) :: Int
          let filename = args !! 2
          let points = [(x,y) | y <-[1..height], x<- [1..width] ]
          let mpoints = map (transform width height) points
          let mbrot = map pointCheck mpoints
          let mfile = PPM {
                      pWidth = width,
                      pHeight = height,
                      pName = filename,
                      pImage = mbrot
                      }
          write mfile
          return ()