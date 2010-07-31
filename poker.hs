import IO
import Data.List ( group, groupBy, sort, sortBy )
import Data.Ord ( comparing )
import System.Random

data Hand = Full_house | Four_of_a_kind | Three_of_a_kind | Two_pair | Two_of_a_kind | No_hand
     deriving (Show, Eq, Ord)

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

print_state context hand deck = do
    print $ context ++ ": hand = " ++ (show hand)
    print $ context ++ ": deck = " ++ (show deck)

do_discarding :: [Card] -> [Card] -> IO [Card]
do_discarding orig_hand hand = do
    let print_hand  = show $ map (\x -> if elem x hand then x else BlankCard) orig_hand
    putStr $ print_hand ++ " - Discard: "
    input <- getLine
    if length input == 0 then return hand else
        let discard     = read input
            card        = orig_hand !! (discard - 1)
            new_hand    = filter (\x -> x /= card) hand in
        do_discarding orig_hand new_hand

main :: IO ()
main = do hSetBuffering stdout NoBuffering 
          stdGen <- getStdGen 
          play_game [] stdGen

play_game :: [Card] -> StdGen -> IO ()
play_game deck_arg stdGen = do
    putStrLn $ "Cards in deck: " ++ (show $ length deck_arg)
    deck <- if length deck_arg < 10 
              then do
                   putStrLn ("Shuffling...")
                   new_deck <- do_shuffle
                   putStrLn ("Cards in deck: " ++ (show $ length new_deck))
                   return new_deck
              else return deck_arg
{-
    let deck1 = if length deck_arg < 10
                   then do_shuffle
                   else return deck_arg
 -}
    (hand, deck) <- do_deal 5 deck
    hand <- do_discarding hand hand
    (new_cards, deck) <- do_deal (5 - length hand) deck
    let new_hand = hand ++ new_cards
    putStrLn $ (show hand) ++ " + " ++ (show new_cards) ++ " = " ++ (show new_hand)
    putStrLn $ "group_hand -> " ++ (show $ group_hand new_hand)
    putStrLn $ "You have: " ++ (show $ best_hand new_hand)
    putStr "Play again ([y], n)? "
    char <- getLine
    if char == "n" then return () else play_game deck stdGen

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
                  4 -> Just ("*** Four of a kind", [biggest_group])
                  3 -> case length second_biggest_group of
                            2 -> Just ("*** Full house", [biggest_group, second_biggest_group])
                            _ -> Just ("*** Three of a kind", [biggest_group])
                  2 -> case length second_biggest_group of
                            2 -> Just ("*** Two pair", [biggest_group, second_biggest_group])
                            _ -> Just ("*** Two of a kind", [biggest_group])
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

test = 
     let hands = [
                   [ "AS", "AH", "KC", "QD", "JH" ]    -- Two_of_a_kind
                 , [ "AS", "AH", "KC", "KD", "JH" ]    -- Two_pair
                 , [ "AS", "AH", "AC", "QD", "JH" ]    -- Three_of_a_kind
                 , [ "AS", "AH", "AC", "QD", "QH" ]    -- Full_house
                 , [ "AS", "AH", "AC", "AD", "JH" ]    -- Four_of_a_kind
                 , [ "2S", "3H", "4C", "5D", "6H" ]    -- Straight (2 to 6)
                 , [ "2H", "3H", "8H", "5H", "6H" ]    -- Flush (Hearts)
                 , [ "2D", "3D", "8D", "5D", "6D" ]    -- Flush (Diamonds)
                 , [ "2C", "3C", "8C", "5C", "6C" ]    -- Flush (Clubs)
                 , [ "2S", "3S", "8S", "5S", "6S" ]    -- Flush (Spades)
{--
 -- The old way of representing cards was kinda verbose and annoying:                
 --
                   [ ("A", "S"), ("A", "H"), ("K", "C"), ("Q", "D"), ("J", "H") ]    -- Two_of_a_kind
                 , [ ("A", "S"), ("A", "H"), ("K", "C"), ("K", "D"), ("J", "H") ]    -- Two_pair
                 , [ ("A", "S"), ("A", "H"), ("A", "C"), ("Q", "D"), ("J", "H") ]    -- Three_of_a_kind
                 , [ ("A", "S"), ("A", "H"), ("A", "C"), ("Q", "D"), ("Q", "H") ]    -- Full_house
                 , [ ("A", "S"), ("A", "H"), ("A", "C"), ("A", "D"), ("J", "H") ]    -- Four_of_a_kind
 -}
{--
 -- The even older way of representing cards was even more verbose and annoying:                
 --
                   [Card Ace Spades, Card Ace Hearts, Card King Clubs, Card Queen Diamonds, Card Jack Hearts]    -- Two_of_a_kind
                 , [Card Ace Spades, Card Ace Hearts, Card King Clubs, Card King Diamonds, Card Jack Hearts]     -- Two_pair
                 , [Card Ace Spades, Card Ace Hearts, Card Ace Clubs, Card Queen Diamonds, Card Jack Hearts]     -- Three_of_a_kind
                 , [Card Ace Spades, Card Ace Hearts, Card Ace Clubs, Card Queen Diamonds, Card Queen Hearts]    -- Full_house
                 , [Card Ace Spades, Card Ace Hearts, Card Ace Clubs, Card Ace Diamonds, Card Jack Hearts]       -- Four_of_a_kind
                 , [Card (NumericRank 9) Spades, Card (NumericRank 10) Hearts, Card Jack Clubs, Card Queen Diamonds, Card King Hearts]   -- Straight
 --}
                 ] in
         test_hands hands
