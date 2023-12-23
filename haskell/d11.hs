import Reader (parseDay)

type Map = [(Int, Int)]

parseLine :: Int -> String -> Map
parseLine y = map fst . filter ((=='#') . snd) . zipWith (\i c -> ((y,i),c)) [0..]

empties :: Map -> ([Int], [Int])
empties m =
    let rows = filter (not . (`elem` map fst m)) [1..(length m)]
        cols = filter (not . (`elem` map snd m)) [1..(length m)]
    in (rows, cols)

distance :: Int -> ([Int], [Int]) -> (Int, Int) -> (Int, Int) -> Int
distance emptySize (rows, cols) (x0,y0) (x1,y1) =
    let xm = min x0 x1
        xM = max x0 x1
        ym = min y0 y1
        yM = max y0 y1
        nrows = (emptySize*) . length . filter (\x -> x > xm && x < xM) $ rows
        ncols = (emptySize*) . length . filter (\y -> y > ym && y < yM) $ cols
    in xM - xm + yM - ym + nrows + ncols


-- ncoords m (rows, cols) = 
--     let npos_rec acc i n l = case l of
--             [] -> reverse acc
--             e:tl -> if i < e then 

makePairs :: [a] -> [(a,a)]
makePairs = foldl (\acc l -> acc ++ zip l (tail l)) [] . iterate tail

allDistances :: Int -> ([Int], [Int]) -> [(Int, Int)] -> Int
allDistances emptySize empt =
    let allDistances_rec s l = case l of
            [] -> s
            [_] -> s
            e:tl -> allDistances_rec (s + sum (map (distance emptySize empt e) tl)) tl
    in allDistances_rec 0

part1 :: ([Int], [Int]) -> [(Int, Int)] -> Int
part1 = allDistances 1

part2 :: ([Int], [Int]) -> [(Int, Int)] -> Int
part2 = allDistances 999999

main :: IO ()
main = do
    rmap <- parseDay "d11" parseLine
    let map = concat rmap
        empt = empties map
    print $ part1 empt map
    print $ part2 empt map