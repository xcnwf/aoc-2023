import Reader ( parseDay )
import Data.Bifunctor ( bimap )

parseLine :: String -> [Int]
parseLine = map read . tail . words

type Challenge = (Int, Int)

rootsChallenge :: Challenge -> (Int, Int)
rootsChallenge (time, distance) = 
    let a = -1.0
        b = fromIntegral time
        c = fromIntegral $ -distance-1 --Add -1 to be sure to best the distance
        deltaSqrt = sqrt(b*b-4*a*c)
    in bimap ceiling floor  ((-b + deltaSqrt)/2/a, (-b - deltaSqrt)/2/a) 

part1 :: [Challenge] -> Int
part1 = 
    product . map ((\(x,y) -> y-x+1) . rootsChallenge)

parseLine2 :: String -> Int
parseLine2 = read . concat . tail . words

part2 :: Challenge -> Int
part2 = (\(x,y) -> y-x+1) . rootsChallenge

main = do 
    [times, distances] <- parseDay "d6" (const parseLine)
    let challenges = zip times distances
    print $ part1 challenges
    [time, distance] <- parseDay "d6" (const parseLine2)
    print $ part2 (time, distance)

