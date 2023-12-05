import Reader (parseDay)
import Data.Char (isDigit)
import Data.List.Split ( splitOn, chunksOf )
import Data.Either (fromLeft, fromRight)
import Data.Maybe (fromJust, fromMaybe)
import Data.List (sortBy, minimumBy)

type RangeMap = (Int, Int, Int)
type Range = (Int, Int)

parseLine :: Int -> String -> Either [Int] (Maybe RangeMap)
parseLine i l
    | i == 0 = Left $ map read . drop 1 . words $ l
    | null l || (not . isDigit . head $ l) = Right Nothing
    | otherwise = let [a, b, c] = map read . words $ l in Right $ Just (a,b,c)

updateSeed :: [RangeMap] -> Int -> Int
updateSeed rs s =
    case filter (\(dst,src,len) -> s >= src && s < (src + len)) rs of
        [] -> s
        (dst,src,len):tl -> dst + (s-src)

part1 :: [Int] -> [[RangeMap]] -> Int
part1 seeds ranges =
    minimum $ foldl (\sds r -> map (updateSeed r) sds) seeds ranges


updateRanges :: [Range] -> [RangeMap] -> [Range]
updateRanges ranges rangemaps  =
    let updateRanges_rec acc ranges rangemaps = case (ranges, rangemaps) of
            (_,[]) -> reverse acc ++ ranges
            ([], hd:tl) -> reverse acc
            (h1@(x1,y1):t1, h2@(x2,y2,v2):t2) ->
                if x1 < x2
                    then if y1 < x2
                        then updateRanges_rec (h1:acc) t1 rangemaps
                        else if y1 >= y2
                            then updateRanges_rec ((v2,v2+(y2-x2)):(x1,x2-1):acc) ((y2+1,y1):t1) t2
                            else updateRanges_rec ((v2,v2+(y1 - x2)):(x1,x2-1):acc) t1 ((y1+1,y2,v2):t2)
                    else if x1 > y2
                        then updateRanges_rec acc ranges t2
                        else if y2 >= y1
                            then updateRanges_rec ((v2,v2+(y1-x1)):acc) t1 ((y1+1,y2,v2):t2)
                            else updateRanges_rec ((v2,v2+(y2-x1)):acc) ((y2+1,y1):t1) t2

    in sortBy (\(a,_) (b,_) -> compare a b) $ updateRanges_rec [] ranges rangemaps

part2 :: [Range] -> [[RangeMap]] -> Int
part2 s = minimum . map fst . foldl updateRanges s

main = do
    content <- parseDay "d5" parseLine
    let seeds = fromLeft [] . head $ content
        ranges = map (map $ fromJust . fromRight (Just (0,0,0))) . splitOn [Right Nothing , Right Nothing] . drop 3 $ content
    print $ part1 seeds ranges
    let cmpRange (a, _, _) (b, _, _) = compare a b
        seedRanges = sortBy (\(a,_) (b,_) -> compare a b) . map (\[x, y] -> (x,x+y-1)) $ chunksOf 2 seeds
        nRanges = map (sortBy cmpRange . map (\(dst,src,len) -> (src, src+len-1, dst))) ranges
    print $ part2 seedRanges nRanges