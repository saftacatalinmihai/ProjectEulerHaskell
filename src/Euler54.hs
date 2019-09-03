module Euler54 where
import Data.List
import Data.Ord
import Data.Maybe
import Utils
import Primes

(|>) x f = f x

main = print test


test = 1 == 1

card num suite = (num, suite)

num (num, suite) = num
suite (num, suite) = suite

hand c1 c2 c3 c4 c5 = [c1, c2, c3, c4, c5]

handNums hand = sort $ map num hand
handSuites hand = map suite hand

highCard hand = maximum $ handNums hand

isFlush hand =
    all ( == head suites ) $ tail suites
    where
        suites = handSuites hand

-- isFlushTest =
--     isFlush h == True
--     where
--         h = hand (card 2 'd') (card 2 's') (card 2 'h') (card 2 'd') (card 2 'd')

isStraight hand =
    [firstNum .. firstNum + 4] == numsSorted
    where
        numsSorted = hand |> map num |> sort
        firstNum = head numsSorted

isStraightFlush hand = isStraight hand && isFlush hand

isRoyalFlush hand = isStraightFlush hand && (head $ handNums hand) == 10

isPair hand =
    n |> group |> any (\l -> length l == 2)
    where
        n = handNums hand

isThreeOfAKind hand =
    n |> group |> any (\l -> length l == 3)
    where
        n = handNums hand

isFourOfAKind hand =
    n |> group |> any (\l -> length l == 4)
    where
        n = handNums hand

isFullHouse hand =
    numGroupLengths == [3,2] || numGroupLengths == [2,3]
    where
        n = handNums hand
        numGroups = group n
        numGroupLengths = map length numGroups

isTwoPairs hand =
    n |> group |> filter (\l -> length l == 2) |> length == 2
    where
        n = handNums hand

data Suit = Club | Diamond | Heart | Spade
     deriving (Read, Show, Enum, Eq, Ord)

data CardValue = Two | Three | Four
     | Five | Six | Seven | Eight | Nine | Ten
     | Jack | Queen | King | Ace
    deriving (Read,  Show, Enum, Eq, Ord)

data Card = Card {value :: CardValue,
                   suit :: Suit}
   deriving (Read, Show, Eq)

instance Ord Card where
     compare c1 c2 = compare (value c1, suit c1) (value c2, suit c2)

instance Enum Card where
     toEnum n  = let (v,s) = n`divMod`4 in Card (toEnum v) (toEnum s)
     fromEnum c = fromEnum (value c) * 4 + fromEnum (suit c)

type Deck = [Card]

deck::Deck
deck = [Card val su | val <- [Two .. Ace], su <- [Club .. Spade]]
