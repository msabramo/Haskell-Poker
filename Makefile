poker : poker.hs
	ghc $< -o $@

clean :
	$(RM) poker poker.o *.hi
