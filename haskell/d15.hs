import Data.List.Extra (wordsBy, deleteBy, sumOn')
import Reader (parseDay)
import Data.Char (ord, digitToInt)
import Debug.Trace (traceShowId)
import Data.Bifunctor (second)

parseLine :: String -> [String]
parseLine = wordsBy (==',')

challHash :: String -> Int
challHash = foldl updateHash 0
    where updateHash prev = (`mod` 256) . (*17) . (+prev) . ord

part1 :: [String] -> Int
part1 = sum . map challHash

type Box = (Int,[(String, Int)])

updateToBoxes :: [Box] -> String -> Int -> [Box]
updateToBoxes bs s v =
    let nbox = challHash s
        updateBoxRec acc ils = case ils of
            [] -> reverse ((s,v):acc)
            i@(k,_):tl -> if k == s then reverse acc ++ ((s,v):tl) else updateBoxRec (i:acc) tl
        updateToBoxesRec acc bls k = case bls of
            [] -> undefined
            b:bs -> if k == nbox then reverse acc ++ (second (updateBoxRec []) b : bs) else updateToBoxesRec (b:acc) bs (k+1)
    in updateToBoxesRec [] bs 0

removeFromBoxes :: [Box] -> String -> [Box]
removeFromBoxes bs s =
    let nbox = challHash s
        removeItemRec acc ils = case ils of
            [] -> reverse acc
            i@(k,_):is -> if k == s then reverse acc ++ is else removeItemRec (i:acc) is
        removeFromBoxesRec acc bls k = case bls of
            [] -> undefined
            b:bs -> if k == nbox then reverse acc ++ (second (removeItemRec []) b : bs) else removeFromBoxesRec (b:acc) bs (k+1)
    in removeFromBoxesRec [] bs 0

part2 :: [String] -> Int
part2 = sum . map score . foldl insertLens (zip [1..256] (repeat []))
    where score (i, l) = sumOn' (i*) $ zipWith (*) [1..] $ map snd l
          insertLens boxes lens =
            let (label, command) = break (\c -> (c=='=') || (c=='-')) lens
                lhash = challHash label
            in case head command of
                '-' -> removeFromBoxes boxes label
                '=' -> let str = digitToInt . head . tail $ command
                        in updateToBoxes boxes label str
                _ -> undefined

main = do
    init <- parseDay "d15" (const parseLine)
    print $ part1 $ head init
    print $ part2 $ head init