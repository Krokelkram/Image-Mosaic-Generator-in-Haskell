module Main where
import Types
import Data.Array
import Text.Printf
import Debug.Trace
import Control.Monad.ST.Lazy
import Data.Array.ST

main :: IO ()
main = do
     let image = runST $ do
                   img <- newArray ((1,1),(1000,1000)) (Pixel 50 50 50) :: ST s (STArray s (Int,Int) Pixel)
                   el <- getElems img
                   return el
     writePPM image (1000,1000) "arrIO.ppm"

writePPM :: [Pixel] -> (Int,Int) -> String -> IO ()
writePPM pxs (w,h) fname = do
  writeFile fname $ printf "P3\n%d %d\n255\n" w h
  trace "Writing..." $ appendFile fname (unwords $ map show (pxs))
