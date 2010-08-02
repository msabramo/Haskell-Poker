run : PokerGame.hs
	runghc $<

PokerGame : PokerGame.hs PlayingCards.hs PokerHands.hs
	ghc --make $@

clean :
	$(RM) PokerGame *.o *.hi
