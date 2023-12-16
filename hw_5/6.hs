module Desk where
import Permute (permute) -- from 3.hs
import Control.Monad
import Text.Printf


-- -----/ Cards /-----

data Suit = Spade | Heart | Diamond | Club
  deriving (Show, Enum)

data Value =
    Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
    | Jack | Queen | King | Ace
  deriving (Show, Enum)

type Card = (Suit, Value)
type Deck = [Card]

makeDeck :: Deck
makeDeck = do
  suit <- [Spade ..]
  value <- [Two ..]
  return (suit, value)

shuffle :: Deck -> IO Deck
shuffle = permute

takeCard :: Deck -> (Card, Deck)
takeCard [] = error "Deck is empty!"
takeCard (x:xs) = (x, xs)

take6Cards :: Deck -> (Deck, Deck)
take6Cards deck
  | length deck <= 6  = (deck, [])
  | otherwise         = splitAt 6 deck


-- -----/ Comparing aces /-----

aceCount :: Deck -> Int
aceCount [] = 0
aceCount [(_,Ace)] = 1
aceCount [(_,  _)] = 0
aceCount (card:cards) = aceCount [card] + aceCount cards

compareAceCounts :: Deck -> Deck -> Ordering
compareAceCounts p1 p2 = compare (aceCount p1) (aceCount p2)

isNotEqualAceCounts :: (Deck, Deck) -> Bool
isNotEqualAceCounts (p1, p2)
  | compareAceCounts p1 p2 == EQ  = False
  | otherwise                     = True

getProbEqualAceCounts :: Int -> IO Double
getProbEqualAceCounts num = do
  decks <- sequence . replicate num $ shuffle makeDeck
  let players = map ((\(a, b) -> (take 6 b, a)) . splitAt 6) decks :: [(Deck, Deck)]
  let not_equal_aces_games_count = length . filter isNotEqualAceCounts $ players
  print (not_equal_aces_games_count, num)
  return $ fromIntegral not_equal_aces_games_count / fromIntegral num


-- -----/ Main /-----

main = forM_ [5, 10, 100, 1000, 10000, 100000] $
  \n -> printf "%6d %f\n" n =<< getProbEqualAceCounts n

-- The analytical solution gives the result:
-- 30206 % 54145
-- equals to 0.557872

-- Output
{-
*Desk> main
(2,5)
     5 0.4
(4,10)
    10 0.4
(53,100)
   100 0.53
(575,1000)
  1000 0.575
(5574,10000)
 10000 0.5574
-}
