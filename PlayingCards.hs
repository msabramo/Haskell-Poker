module PlayingCards where

import Data.List ( sortBy )
import Data.Ord ( comparing )
import System.Random

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace
     deriving (Eq, Ord, Enum)

instance Show Rank where
         show Two               = "2"
         show Three             = "3"
         show Four              = "4"
         show Five              = "5"
         show Six               = "6"
         show Seven             = "7"
         show Eight             = "8"
         show Nine              = "9"
         show Ten               = "10"
         show Jack              = "J"
         show Queen             = "Q"
         show King              = "K"
         show Ace               = "A"

data Suit = Hearts | Diamonds | Clubs | Spades 
     deriving (Show, Eq)

data Card = Card { rank :: Rank, suit :: Suit } | BlankCard
     deriving (Eq)

string_to_card str =
              let first = head str
                  second = last str
                  rank = case first of
                              '2'  -> Two
                              '3'  -> Three
                              '4'  -> Four
                              '5'  -> Five
                              '6'  -> Six
                              '7'  -> Seven
                              '8'  -> Eight
                              '9'  -> Nine
                              '1' -> Ten
                              'J'  -> Jack
                              'Q'  -> Queen
                              'K'  -> King
                              'A'  -> Ace
                  suit = case second of
                              'H' -> Hearts
                              'D' -> Diamonds
                              'C' -> Clubs
                              'S' -> Spades 
              in Card rank suit

instance Show Card where
         show BlankCard = "--"
         show card = show (rank card) ++ [head $ show $ suit card]

all_cards :: [Card]
all_cards = [ Card rank suit
            | suit <- [Hearts, Diamonds, Clubs, Spades]
            , rank <- [Two .. Ace] ]

shuffle :: StdGen -> [a] -> [a]
shuffle g es = map snd $ sortBy (comparing fst) $ zip rs es
               where rs = randoms g :: [Integer]

do_shuffle :: IO [Card]
do_shuffle = do gen <- getStdGen
                return (shuffle gen all_cards)

deal :: Int -> [Card] -> ([Card], [Card])
deal n deck = splitAt n deck

do_deal:: Int -> [Card] -> IO ([Card], [Card])
do_deal n deck = do return (deal n deck)

