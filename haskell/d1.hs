{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Data.Char ( digitToInt, isDigit )
import Data.Maybe ( fromMaybe )
import Control.Applicative ( Alternative((<|>)) )
import Reader ( parseDay )
import Data.List (isPrefixOf, reverse)
import qualified Data.Bifunctor

update_fst_lst :: (Maybe Int, Maybe Int) -> Char -> (Maybe Int, Maybe Int)
update_fst_lst (x,y) a = let e = if isDigit a then Just (digitToInt a) else Nothing in (x <|> e, e <|> y)

fst_lst_digits :: String -> (Maybe Int, Maybe Int)
fst_lst_digits = foldl update_fst_lst (Nothing, Nothing)

part1 :: [String] -> Int
part1 content =
    let (tens, digits) = unzip $ map fst_lst_digits content
        d = fromMaybe 0
        numbers = zipWith (\a b -> 10 * d a + d b) tens digits
    in sum numbers

litterals :: [(String, Int)]
litterals = zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "1", "2", "3","4", "5", "6", "7", "8", "9"] ([1..9]++[1..9])

rlitterals :: [(String, Int)]
rlitterals = uncurry zip $ Data.Bifunctor.first (map reverse) $ unzip litterals

getFirst :: [(String, Int)] -> String -> Int
getFirst subs line =
    let getFirst_rec t = case t of
            [] -> 0
            _ -> case foldr (<|>) Nothing $ map (\(pr, i) -> if (pr `isPrefixOf` t) then Just i else Nothing) subs of
                Just x -> x
                Nothing -> getFirst_rec (tail t)
    in getFirst_rec line


part2 :: [String] -> Int
part2 content =
    let fsts = map (getFirst litterals) content
        snds = map (getFirst rlitterals . reverse) content
    in sum $ zipWith (\a b -> 10 * a + b) fsts snds

main = do
    text <- parseDay "d1" (const id)
    print . part1 $ text
    print . part2 $ text