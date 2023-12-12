import Reader ( parseDay )
import Data.Bifunctor ( first )
import Debug.Trace (traceShowId)


parseLine :: String -> [Int]
parseLine = map read . words

nextToken :: [Int] -> Int
nextToken = 
    let getdiffs =
            let diffsRec (acc, last) next = case next of 
                    x:y:tl -> diffsRec (y-x:acc,y) (y:tl)
                    _ -> (acc, last)
            in first reverse . diffsRec ([],0)
        nextTokenRec lasts diffs = 
            if all (==0) diffs then sum lasts else 
            let (ndiffs, nlast) = getdiffs diffs 
            in nextTokenRec (nlast:lasts) ndiffs
    in nextTokenRec []

part1 :: [[Int]] -> Int
part1 = sum . traceShowId . map nextToken

main = do
    progressions <- parseDay "d9" (const parseLine)
    print . part1 $ progressions
    print . part1 . map reverse $ progressions

