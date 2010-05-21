import Control.Monad.ST.Lazy
import Data.Array.ST

data Foo = Foo {
     arr :: ST s (STArray s Int Int) -- no idea what type this should be
     }

useArray :: ST s Int
useArray = do
  newArr <- newArray (1,10) 77 :: ST s (STArray s Int Int)
  let bar = Foo {
            arr = newArr -- or maybe a direct newArray? would try both, but doesn't work anyway...
            }
  a <- readArray (arr bar) 2  
  return a

main :: IO ()
main = do
  print $ runST useArray
  