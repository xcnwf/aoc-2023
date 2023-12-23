import Reader (parseDay)
import Data.List (group, groupBy, transpose)
import Data.Maybe (isJust, isNothing, fromJust, listToMaybe, fromMaybe)
import qualified Data.List.NonEmpty as NE
import Debug.Trace (traceShowId)
import Data.Bifunctor (second)

type Line = [Bool]
type Board = [Line]

parseLine :: [Char] -> Maybe Line
parseLine line = case line of
    [] -> Nothing
    _ -> Just (map (=='#') line)


verticalDifferences :: Line -> Int -> Int
verticalDifferences line n =
    let (l,r) = splitAt n line
    in length $ filter id $ zipWith (/=) r (reverse l)

filterVerticalErrors :: Line -> [(Int,Int)] -> [(Int, Int)]
filterVerticalErrors line =
    filter ((<=2) . snd) . map (\(a,b) -> (a, b + verticalDifferences line a))

verticalErrors :: Board -> [(Int,Int)]
verticalErrors lines =
    let l = length $ head lines
    in foldr filterVerticalErrors (zip [1..(l-1)] $ repeat 0) lines

horizontalErrors :: Board -> [(Int, Int)]
horizontalErrors = verticalErrors . transpose

getBoardScore :: Int -> Board -> Int
getBoardScore error board = fromMaybe ((100*) . head . filterMapRefs $ horizontalErrors board) $ listToMaybe . filterMapRefs $ verticalErrors board
    where filterMapRefs = map fst . filter ((==error).snd)

part1 :: [Board] -> Int
part1 = sum . traceShowId . map (getBoardScore 0)

part2 :: [Board] -> Int
part2 = sum . map (getBoardScore 1)

main = do
    content <- parseDay "d13" (const parseLine)
    let boards = map (map fromJust . NE.toList) $ filter (isJust . NE.head) $ NE.groupWith isJust content
    print $ part1 boards
    print $ part2 boards