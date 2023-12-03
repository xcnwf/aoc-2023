module Reader where
import System.IO
import Control.Monad

parseDay :: String -> (Int -> String -> a) -> IO [a]
parseDay filename f = do
    contents <- readFile ("../inputs/"++filename++".in")
    let lines_arr = lines contents
        list = zipWith f [0..] lines_arr
    return list