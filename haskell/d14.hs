import Reader (parseDay)
import Data.List.Extra (concatUnzip, sortOn, headDef, sort)
import Data.Bifunctor (Bifunctor(bimap))
import Debug.Trace (traceShowId)
import qualified Data.HashMap.Strict as HM


type Location = (Int, Int) --(y,x), with top left being 0,0 
parseLine :: Int -> String -> ([Location], [Location]) -- Solid and moving boulders 
parseLine n =
    let addBoulders (static, dynamic) (i,c) = case c of
            '#' -> ((n,i):static, dynamic)
            'O' -> (static, (n,i):dynamic)
            _ -> (static, dynamic)
    in bimap reverse reverse . foldl addBoulders ([],[]) . zip [0..]

updateBouldersNorth m st acc (y,x) =
    let northBlocker = headDef (-1) . reverse . sort . map fst . filter (\(a,b) -> b==x && a<y) $ acc ++ st
    in (northBlocker + 1,x):acc
updateBouldersSouth m st acc (y,x) =
    let southBlocker = headDef m . sort . map fst . filter (\(a,b) -> b==x && a>y) $ acc ++ st
    in (southBlocker - 1,x):acc
updateBouldersWest m st acc (y,x) =
    let westBlocker = headDef (-1) . reverse . sort . map snd . filter (\(a,b) -> a==y && b<x) $ acc ++ st
    in (y,westBlocker + 1):acc
updateBouldersEast m st acc (y,x) =
    let eastBlocker = headDef m . sort . map snd . filter (\(a,b) -> a==y && b>x) $ acc ++ st
    in (y,eastBlocker - 1):acc

spin :: Int -> Int -> [Location] -> [Location] -> [Location]
spin h w st =
    foldl (updateBouldersEast w st) [] . sortOn (negate.snd) .
    foldl (updateBouldersSouth h st) [] . sortOn (negate.fst) .
    foldl (updateBouldersWest w st) [] . sortOn snd .
    foldl (updateBouldersNorth h st) [] . sortOn fst

score :: Int -> [Location] -> Int
score h = sum . map ((h-) . fst)

part1 :: Int -> [Location] -> [Location] -> Int
part1 m st = sum . map boulderScore . foldl (updateBouldersNorth m st) []
    where boulderScore = (m-) . fst

part2 :: Int -> Int -> [Location] -> [Location] -> Int
part2 h w st =
    let spinRec m ts i dyn =
            case HM.lookup dyn m of
                Just prev -> 
                    let next = prev + ((1000000000 - prev) `mod` (i - prev))
                    in reverse ts !! next
                Nothing ->
                     let ndyn = spin h w st dyn
                         s = score h dyn
                     in spinRec (HM.insert dyn i m) (s:ts) (i+1) ndyn
    in spinRec HM.empty [] 0

main = do
    lines <- parseDay "d14" (,)
    let h = length lines
        w = length $ snd $ head lines
        boulders = map (uncurry parseLine) lines
        (static, dynamic) = concatUnzip boulders
    print $ part1 h static dynamic
    print $ part2 h w static dynamic