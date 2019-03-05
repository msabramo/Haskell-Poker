import System.IO
import System.Random

import PlayingCards
import PokerHands

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
