
import Reader (parseDay)
import Data.List.Split (splitOn)
import qualified Data.Bifunctor
import Debug.Trace (traceShowId)

type Game = (Int, [[(Int, String)]])

parseLine :: String -> Game
parseLine line =
    let (d, r) = Data.Bifunctor.bimap (read::String->Int ) (drop 2) $ break (==':') . drop 5 $ line
        takes = map (map (Data.Bifunctor.bimap (read::String->Int) (drop 1) . break (==' ')) . splitOn ", ") $ splitOn "; " r
    in (d, takes)

maxColors :: [[(Int, String)]] -> (Int, Int, Int)
maxColors =
    let maxCol (i,c) (r, g, b) = (case c of
            "red" -> (max i r, g, b)
            "green" -> (r, max i g, b)
            "blue" -> (r, g, max i b)
            _ -> (-1,-1,-1))
    in foldr maxCol (0,0,0) . concat

part1 :: [Game] -> Int
part1 = sum . map fst . filter ((\(r,g,b) -> r<=12&&g<=13&&b<=14) . maxColors . snd)

part2 :: [Game] -> Int
part2 = sum . map ((\(r,g,b) -> r*g*b) . maxColors . snd)

main = do
    values <- parseDay "d2" (const parseLine)
    print $ part1 values
    print $ part2 values