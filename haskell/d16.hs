import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (catMaybes)
import Reader (parseDay)
import Data.Hashable (Hashable, hashWithSalt, hashUsing)
import Prelude hiding (Left, Right)

type Location = (Int, Int) -- (y,x), with (0,0) being top left
data Mirror = RightMirror | LeftMirror | HorizontalSplitter | VerticalSplitter deriving (Enum, Eq)
data Direction = Left | Right | Up | Down deriving (Enum, Eq)
instance Hashable Direction where
    hashWithSalt = hashUsing fromEnum
type Mirrors = HM.HashMap Location Mirror

parseLine :: Int -> String -> Mirrors
parseLine y = HM.fromList . catMaybes . zipWith (\x c -> (,) (y,x) <$> parseChar c) [0..]
    where parseChar c = case c of
            '/' -> Just RightMirror
            '\\' -> Just LeftMirror
            '|' -> Just VerticalSplitter
            '-' -> Just HorizontalSplitter
            _ -> Nothing

type Ray = (Location, Direction)
type Visited = HS.HashSet Ray

moveRay :: Ray -> Ray
moveRay ((y,x),dir) = 
    let nloc = case dir of
            Left -> (y,x-1)
            Right -> (y,x+1)
            Up -> (y-1,x)
            Down -> (y+1,x)
    in (nloc,dir)

modifyRay :: Int -> Int -> Mirrors -> [Ray] -> Ray -> [Ray]
modifyRay h w mirrors nextRays r@((y,x),dir) = 
  let nrays = case HM.lookup (y, x) mirrors of
        Just RightMirror ->
            let ndir = case dir of
                    Left -> Down
                    Right -> Up
                    Up -> Right
                    Down -> Left
            in [((y,x),ndir)]
        Just LeftMirror -> 
            let ndir = case dir of
                    Left -> Up
                    Right -> Down
                    Up -> Left
                    Down -> Right
            in [((y,x),ndir)]
        Just HorizontalSplitter -> if dir == Left || dir == Right then [r] else [((y,x),Right),((y,x),Left)] 
        Just VerticalSplitter -> if dir == Up || dir == Down then [r] else [((y,x),Up),((y,x),Down)]
        Nothing -> [r]
      nraysMoved = map moveRay nrays
      nraysFiltered = filter (\((y,x),_) -> y >= 0 && y < h && x >= 0 && x < w) nraysMoved
  in nraysFiltered ++ nextRays

simulateLight :: Ray -> Int -> Int -> Mirrors -> Visited
simulateLight startRay h w mirrors = 
    let continueRay visited activeRays = case activeRays of
            [] -> visited
            r@((y,x),dir):tl -> if HS.member r visited
                then continueRay visited tl 
                else continueRay (HS.insert r visited) (modifyRay h w mirrors tl r)
    in continueRay HS.empty [startRay]

totalEnergized :: Ray -> Int -> Int -> Mirrors -> Int
totalEnergized startRay h w mirrors = length . HS.map fst $ simulateLight startRay h w mirrors

part1 :: Int -> Int -> Mirrors -> Int
part1 = totalEnergized ((0,0), Right)

part2 :: Int -> Int -> Mirrors -> Int
part2 h w mirrors = 
    let zipLocDir = zipWith3 (\y x dir ->((y,x),dir))
        allStartPoints = 
            zipLocDir (repeat 0) [0..(w-1)] (repeat Down) ++
            zipLocDir (repeat (h-1)) [0..(w-1)] (repeat Up) ++
            zipLocDir [0..(h-1)] (repeat 0) (repeat Right) ++            
            zipLocDir [0..(h-1)] (repeat (w-1)) (repeat Left)
    in maximum $ map (\x -> totalEnergized x h w mirrors) allStartPoints

printMirrors :: Int -> Int -> Mirrors -> IO ()
printMirrors h w mirrors = 
    let mirrorChar acc (y,x) = case HM.lookup (y, x) mirrors of
            Just RightMirror -> '/':acc
            Just LeftMirror -> '\\':acc
            Just VerticalSplitter -> '|':acc
            Just HorizontalSplitter -> '-':acc
            Nothing -> '.':acc
    in foldl (\io y ->  io >> putStr (reverse . ('\n':) . foldl mirrorChar [] . map ((,) y) $ [0..(w-1)])) (Prelude.return ()) [0..(h-1)]

printVisited :: Int -> Int -> Visited -> IO ()
printVisited h w visited = 
    let visitedCases = HS.map fst visited
    in foldl (\io y -> io >> putStrLn (map (\x -> if HS.member (y,x) visitedCases then '#' else '.') [0..(w-1)])) (Prelude.return ()) [0..(h-1)]

main = do 
    lines <- parseDay "d16" (,)
    let h = length lines
        w = length . snd . head $ lines
        mirrors = foldl1 HM.union . map (uncurry parseLine) $ lines
    print $ part1 h w mirrors
    print $ part2 h w mirrors