module Reader where
import System.IO  
import Control.Monad

parseDay :: String -> (String -> a) -> IO [a]
parseDay filename f = do 
    contents <- readFile ("../inputs/"++filename++".in")
    let lines_arr = lines contents
        list = map f lines_arr
    return list