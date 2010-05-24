{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module MosaicImage where
import Text.Printf
import Control.Monad.ST.Lazy
import Data.Array.ST

class Image a s where
    get :: a -> (Int, Int) -> ST s Pixel
    set :: a -> (Int, Int) ->  Pixel -> ST s ()
    width :: a -> Int
    height :: a -> Int

data PPMImage s = PPMImage {
     ppmArray :: STArray s (Int,Int) Pixel
    ,ppmWidth :: Int
    ,ppmHeight :: Int
    ,ppmName :: String
    }

data SubImage s = SubImage {
     parent :: PPMImage s
    ,offsetX :: Int
    ,offsetY :: Int
    ,subWidth :: Int
    ,subHeight :: Int
    }

instance Image (PPMImage s) s where
    get img (x,y) = do
      readArray (ppmArray img) (x,y) 
    set img (x,y) px = do
      writeArray (ppmArray img) (x,y) px
    -- just to define them
    width _  = 42
    height _ = 42

instance Image (SubImage s) s where
    get (SubImage parent x' y' _ _) (x,y) = 
        let (rx, ry) = (x'+x , y'+y)
        in get parent (rx,ry)
    set (SubImage parent x' y' _ _) (x,y) =
        let (rx, ry) = (x'+x , y'+y)
        in set parent (rx,ry)
    width _  = 42
    height _ = 42
      
data Pixel = Pixel {
      pR :: Int
    , pG :: Int
    , pB :: Int
    } deriving Eq

-- Shows contents of a pixel as string.
instance Show Pixel where
    show (Pixel r g b) = printf "%d %d %d" r g b