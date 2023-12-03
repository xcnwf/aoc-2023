import Reader (parseDay)
import Data.Bifunctor (bimap)
import Data.Char (isDigit)
import Data.List (span)

data SNum = SNum {
    value :: Int,
    y :: Int,
    xl :: Int,
    xr :: Int
} deriving Show

parseLine :: Int -> String -> ([SNum], [(Char,(Int, Int))])
parseLine y = 
    let parseNumber t = 
            let (l,r) = span isDigit t
            in ((read::String -> Int) l, r, length l)
        parseLine_rec (nums,syms) i t = 
            case t of
                [] -> (nums, syms)
                x:tl -> case x of 
                    '.' -> parseLine_rec (nums, syms) (i+1) tl
                    x 
                        | isDigit x -> 
                            let (num,r,len) = parseNumber t
                                n = SNum num y i (i+len-1)
                            in parseLine_rec (n:nums, syms) (i+len) r
                        | otherwise -> parseLine_rec (nums, (x,(y,i)):syms) (i+1) tl
    in parseLine_rec ([],[]) 0

part1 :: ([SNum], [(Char, (Int, Int))]) -> Int
part1 (ln, ls) = 
    let adjacentSymbol l (SNum _ y xl xr) = any ((\(ys,xs) -> abs (y-ys) <= 1 && xs >= xl - 1 && xs <= xr + 1) . snd) l
    in sum . map value . filter (adjacentSymbol ls) $ ln

part2 :: ([SNum], [(Char, (Int, Int))]) -> Int
part2 (ln,ls) = 
    let stars = filter ((=='*') . fst) ls
        neighbours (ys,xs) = filter (\(SNum _ y xl xr) -> abs (y-ys) <= 1 && xs >= xl - 1 && xs <= xr + 1) ln
    in sum . map (product . map value) . filter ((==2) . length) . map (neighbours . snd) $ stars

main = do  
    text <- readFile "../inputs/d3.in"
    let results = foldr ((\(l1,l2) (n,s) -> (l1++n,l2++s)) . uncurry parseLine) ([],[]) $ zip [0..] $ lines text
    print $ part1 results
    print $ part2 results
