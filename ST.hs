import Control.Monad.ST.Lazy
import Data.Array.ST

-- The s "encapsulates" the internal states, i.e. takes care that is it not
-- transported to the outside. Try to change example to
--
--   ...
--   return store
--
-- remove the type signature and take a look at its type using :t.
data Store s = Store {
     storage :: STArray s Int Int
}

example :: ST s Int
example = do
    array <- newArray (1,10) 42 :: ST s (STArray s Int Int)
    let store = Store array
    a <- readArray (storage store) 2  
    return a

main :: IO ()
main = print $ runST example

