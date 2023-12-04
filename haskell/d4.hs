import Reader (parseDay)
import Data.Bifunctor ( bimap, second )
import Debug.Trace (traceShowId)

type Card = ([Int],[Int])

parseLine :: String -> Card
parseLine = bimap (map read) (map read . drop 1) . break (=="|") . words . drop 2 . dropWhile (/=':')

score :: Card -> Int
score = length . \(wins, nums) -> filter (`elem` wins) nums

part1 :: [Card] -> Int
part1 = sum . map ((2^) . subtract 1) . filter (> 0) . map score

addN :: Int -> Int -> [Int] -> [Int]
addN =
    let addN_rec acc i k r = if i <= 0 then reverse acc++r else case r of
         [] -> reverse acc
         e:tl -> addN_rec ((e+k):acc) (i-1) k tl
    in addN_rec []

part2 :: [Card] -> Int
part2 =
    let part2_rec s (acc,multiples) = (acc + head multiples, addN s (head multiples) . drop 1 $ multiples)
    in fst . foldr (part2_rec . score) (0, repeat 1) . reverse


main = do
    cards <- parseDay "d4" (const parseLine)
    print . part1 $ cards
    print . part2 $ cards