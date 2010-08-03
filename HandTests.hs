import Test.HUnit

import PlayingCards
import PokerHands

{-
testTwoOfAKind = TestCase $ assertEqual
  "Two of a kind" 
  (Just ("*** Two of a kind", [hand_from_string_list ["AS", "AH"]]))
  (best_hand $ hand_from_string_list ["AS", "AH", "KC", "QD", "JH"])
-}

{-
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
-}

testTwoOfAKind   = testHand 
                   ["AS", "AH", "KC", "QD", "JH"]    "Two of a kind"
                   [["AS", "AH"]]
testTwoPair      = testHand 
                   ["AS", "AH", "KC", "KD", "JH"]    "Two pair"
                   [["AS", "AH"], ["KC", "KD"]]
testThreeOfAKind = testHand 
                   ["AS", "AH", "AC", "QD", "JH"]    "Three of a kind"
                   [["AS", "AH", "AC"]]
testFullHouse    = testHand 
                   ["AS", "AH", "AC", "QD", "QH"]    "Full house"
                   [["AS", "AH", "AC"], ["QD", "QH"]]
testFourOfAKind  = testHand 
                   ["AS", "AH", "AC", "AD", "JH"]    "Four of a kind"
                   [["AS", "AH", "AC", "AD"]]
testStraight     = testHand
                   ["2S", "3H", "4C", "5D", "6H"]    "Straight"
                   [["AS", "AH", "AC", "AD"]]

testHand cards handName groups = 
    TestCase $ assertEqual
             handName
             (Just (handName, map hand_from_string_list groups))
             (best_hand $ hand_from_string_list cards)

main = runTestTT $ TestList[
        testTwoOfAKind,
        testTwoPair,
        testThreeOfAKind,
        testFullHouse,
        testFourOfAKind,
        testStraight
       ]
