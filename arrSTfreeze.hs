module Main where
import Types
import Data.Array
import Text.Printf
import Debug.Trace
import Control.Monad.ST.Lazy
import Data.Array.ST

main :: IO ()
main = do
     let image = array ((1,1),(1000,1000)) [((x,y), Pixel 50 50 50) | y <- [1..1000], x <- [1..1000]] :: Array (Int,Int) Pixel
     let newImage = runST $ do
                   thawn <- thaw image :: ST s (STArray s (Int,Int) Pixel)
                   -- this is where a tile would be added to the mosaic
                   frozen <- freeze thawn 
                   --el <- elems frozen
                   return frozen :: ST s (Array (Int,Int) Pixel)
     writePPM (elems newImage) (1000,1000) "arrIO.ppm"

writePPM :: [Pixel] -> (Int,Int) -> String -> IO ()
writePPM pxs (w,h) fname = do
  writeFile fname $ printf "P3\n%d %d\n255\n" w h
  trace "Writing..." $ appendFile fname (unwords $ map show (pxs))
