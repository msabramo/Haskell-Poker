module PokerHands where

import Data.List ( groupBy, sortBy )
import Data.Ord ( comparing )

import PlayingCards

{-
data Hand = Full_house | Four_of_a_kind | Three_of_a_kind | Two_pair | Two_of_a_kind | No_hand
     deriving (Show, Eq, Ord)
 -}

group_hand hand =
    let equal_rank        = (\x y -> rank x == rank y)
        groups            = groupBy equal_rank $ sortBy (comparing rank) hand in
        reverse $ sortBy (comparing length) groups

best_hand :: [Card] -> Maybe (String, [[Card]])
best_hand hand =
          rank_matches hand
{-
    case rank_matches hand of
         Just x -> x
         Nothing -> ("Nothing", [])
-}
{-
    let funcs = [ (full_house, "Full house")
                , (four_of_a_kind, "Four of a kind")
                , (three_of_a_kind, "Three of a kind") 
                , (two_pairs, "Two pairs")
                , (two_of_a_kind, "Two of a kind") 
                ] in best_hand_rec hand funcs    
-}

{-
best_hand_rec :: [Card] -> [([Card] -> (Bool, [[Card]]), String)] -> (String, [[Card]])
best_hand_rec hand ((f,name):fs) =
              let (bool, matches) = f hand in
              if bool then (name, matches) else best_hand_rec hand fs
best_hand_rec hand _ = ("No Hand", [])
-}

rank_matches :: [Card] -> Maybe (String, [[Card]])
rank_matches hand =
             let biggest_group          = group_hand hand !! 0
                 second_biggest_group   = group_hand hand !! 1 
             in
             case length biggest_group of
                  4 -> Just ("Four of a kind", [biggest_group])
                  3 -> case length second_biggest_group of
                            2 -> Just ("Full house", [biggest_group, second_biggest_group])
                            _ -> Just ("Three of a kind", [biggest_group])
                  2 -> case length second_biggest_group of
                            2 -> Just ("Two pair", [biggest_group, second_biggest_group])
                            _ -> Just ("Two of a kind", [biggest_group])
                  _ -> Nothing

{-
full_house hand =
    if length (group_hand hand !! 0) == 3 && length (group_hand hand !! 1) == 2 then
        (True, [group_hand hand !! 0, group_hand hand !! 1])
    else
        (False, [])

four_of_a_kind hand =
    if length (group_hand hand !! 0) == 4 then (True, [group_hand hand !! 0]) else (False, [])

three_of_a_kind hand =
    if length (group_hand hand !! 0) == 3 then (True, [group_hand hand !! 0]) else (False, [])

two_of_a_kind hand =
    if length (group_hand hand !! 0) == 2 then (True, [group_hand hand !! 0]) else (False, [])

two_pairs hand =
    if length (group_hand hand !! 0) == 2 && length (group_hand hand !! 1) == 2 then
        (True, [group_hand hand !! 0, group_hand hand !! 1])
    else
        (False, [])
-}

{-
tuple_to_card (first,second) =
              let rank = case first of
                              "2"  -> Two
                              "3"  -> Three
                              "4"  -> Four
                              "5"  -> Five
                              "6"  -> Six
                              "7"  -> Seven
                              "8"  -> Eight
                              "9"  -> Nine
                              "10" -> Ten
                              "J"  -> Jack
                              "Q"  -> Queen
                              "K"  -> King
                              "A"  -> Ace
                  suit = case second of
                              "H" -> Hearts
                              "D" -> Diamonds
                              "C" -> Clubs
                              "S" -> Spades 
              in Card rank suit
 -}

test_hands :: [[String]] -> IO ()
test_hands (hand_tuples:rest) = 
           let hand = map string_to_card hand_tuples
           in do putStrLn ("Hand: " ++ (show hand) ++ " -> " ++ (show $ best_hand hand))
                 test_hands rest
test_hands _ = do putStrLn ("Done testing hands.")

testPokerHands = 
     let hands = [ [ "AS", "AH", "KC", "QD", "JH" ]    -- Two_of_a_kind
                 , [ "AS", "AH", "KC", "KD", "JH" ]    -- Two_pair
                 , [ "AS", "AH", "AC", "QD", "JH" ]    -- Three_of_a_kind
                 , [ "AS", "AH", "AC", "QD", "QH" ]    -- Full_house
                 , [ "AS", "AH", "AC", "AD", "JH" ]    -- Four_of_a_kind
                 , [ "2S", "3H", "4C", "5D", "6H" ]    -- Straight (2 to 6)
                 , [ "2H", "3H", "8H", "5H", "6H" ]    -- Flush (Hearts)
                 , [ "2D", "3D", "8D", "5D", "6D" ]    -- Flush (Diamonds)
                 , [ "2C", "3C", "8C", "5C", "6C" ]    -- Flush (Clubs)
                 , [ "2S", "3S", "8S", "5S", "6S" ]    -- Flush (Spades)
                 ] in
         test_hands hands

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
                              '1'  -> Ten
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

hand_from_string_list strs = map string_to_card strs