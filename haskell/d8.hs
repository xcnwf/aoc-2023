import Reader ( parseDay )
import Data.Bifunctor ( second, bimap )
import Data.Either (fromLeft, fromRight)
import Data.Maybe (catMaybes, fromJust)

type Branch = (String, (String, String))

parseLine :: Int -> String -> Maybe (Either String Branch)
parseLine i l
  | i == 0 = Just (Left l)
  | i == 1 = Nothing
  | otherwise =
    let (name,tl) = splitAt 3 l
        branch = second (drop 2) . splitAt 3 . take 8 . drop 4 $ tl
    in Just(Right(name,branch))

part1 :: String -> [Branch] -> Int
part1 steps branches =
    let part1_rec i currBranch (ns:steps) =
            if currBranch == "ZZZ"
                then i
                else
                    let branchPair = fromJust . lookup currBranch $ branches
                        nextBranch =
                                (case ns of
                                'L' -> fst
                                'R' -> snd
                                _ -> undefined) branchPair
                    in part1_rec (i+1) nextBranch steps
    in part1_rec 0 "AAA" (cycle steps)

main = do
    content <- parseDay "d8" parseLine
    let (steps,assocs) = bimap (fromLeft undefined . head) (map (fromRight undefined)) . splitAt 1 . catMaybes $ content
    print steps
    print assocs
    print $ part1 steps assocs