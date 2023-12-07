import Reader ( parseDay )
import Data.Bifunctor ( bimap, first )
import Data.List ( group, sort, sortBy, sortOn )

type Hand = [Card]
type Player = (Hand, Int)
data Strength = High | Pair | TPair | ToK | Full | Quad | FoK deriving (Enum, Eq, Ord, Show)
data Card = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jockey | Queen | King | Ace deriving (Enum, Eq, Ord, Show)

toCard :: Char -> Card
toCard x
    | x=='2' = Two
    | x=='3' = Three
    | x=='4' = Four
    | x=='5' = Five
    | x=='6' = Six
    | x=='7' = Seven
    | x=='8' = Eight
    | x=='9' = Nine
    | x=='T' = Ten
    | x=='J' = Jockey
    | x=='Q' = Queen
    | x=='K' = King
    | x=='A' = Ace
    | otherwise = undefined

parseLine :: String -> Player
parseLine = bimap (map toCard) read . break (==' ')

handStrength :: Hand -> Strength
handStrength hand =
    let trueCards = filter (/= Joker) hand
        numCards = reverse . sort . map length . group . sort $ trueCards
        nums = case numCards of 
            [] -> [5] -- case of 5 Jokers
            x:tl -> (x + 5-length trueCards):tl
    in case nums of
        [1,1,1,1,1] -> High
        [2,1,1,1] -> Pair
        [2,2,1] -> TPair
        [3,1,1] -> ToK
        [3,2] -> Full
        [4,1] -> Quad
        [5] -> FoK
        _ -> undefined

part1 :: [Player] -> Int
part1 = sum . zipWith (*) [1..] . map snd . sortOn (\(x,_) -> (handStrength x, x))

main = do
    players <- parseDay "d7" (const parseLine)
    print $ part1 players
    let newPlayers = map (first (map (\c -> if c == Jockey then Joker else c))) players
    print $ part1 newPlayers