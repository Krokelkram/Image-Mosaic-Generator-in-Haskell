import Control.Monad.ST.Strict
import Data.Array.ST

class Image a where
    get ::  (Int, Int) -> a -> ST s Int
    set :: (Int, Int) -> a -> Int -> ST s a
    width :: a -> Int
    height :: a -> Int

data PPMImage s = PPMImage {
    ppmArray :: STArray s (Int,Int) Int
    }

-- was muss ich hier anpassen, damit das 's' mitgeht?
-- und falls das so stimmt: der Fehler lautet
-- Could not deduce (MArray (STArray s) Int (ST s1)) from context ()
instance Image (PPMImage s) where
    get (x,y) img = do
      value <- readArray (ppmArray img) (x,y) :: ST s Int
      return value
    set (x,y) img px = do
      value <- writeArray (ppmArray img) (x,y) px
      return value

useArray :: ST s Int
useArray = do
  array <- newArray ((1,1),(10,10)) 42 :: ST s (STArray s (Int,Int) Int)
  let img = PPMImage array
  a <- readArray (ppmArray img) (2,2)
  return a

main :: IO()
main = print $ runST useArray

