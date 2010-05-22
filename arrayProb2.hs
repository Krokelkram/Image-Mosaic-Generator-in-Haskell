{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
import Control.Monad.ST.Strict
import Data.Array.ST

class Image a s where
    -- Makes more sense and eases programming later on when the first
    -- parameter is the actual data instance.
    get     :: a -> (Int, Int) -> ST s Int
    set     :: a -> (Int, Int) -> Int -> ST s ()
    width   :: a -> Int
    height  :: a -> Int


data PPMImage s = PPMImage {
    ppmData :: STArray s (Int,Int) Int
}


main :: IO()
main = print $ runST $ do
    array <- newArray ((1,1),(10,10)) 42 :: ST s (STArray s (Int,Int) Int)
    let image = PPMImage array

    a1 <- readArray (ppmData image) (2,2)
    -- PPMImage is an instance of Image, hence we can also do:
    a2 <- get image (2,2)

    return $ a1 == a2


instance Image (PPMImage s) s where
    get img (x,y) = do
      -- A construct 
      --
      --   value <- monadicOp
      --   return value
      --
      -- is equivalent to
      --
      --   monadicOp
      --
      -- Just look at the types :D
      readArray (ppmData img) (x,y) 
    
    set img (x,y) px = do
        -- writeArray changes the *state* of the array, so it does not return
        -- a value; so, as long as you are inside the particular ST instance,
        -- you actually have mutable state!
        writeArray (ppmData img) (x,y) px

    -- Need to be defined, too.
    width _  = 42
    height _ = 42

