Haskell Poker
=============

This is the result from playing around with Haskell.

# Running

```
$ cabal new-run
Up to date
Cards in deck: 0
Shuffling...
Cards in deck: 52
[JS,3S,10H,AS,JC] - Discard: 2
[JS,--,10H,AS,JC] - Discard:
[JS,10H,AS,JC] + [4H] = [JS,10H,AS,JC,4H]
group_hand -> [[JS,JC],[AS],[10H],[4H]]
You have: Just ("Two of a kind",[[JS,JC]])
Play again ([y], n)?
Cards in deck: 46
[KH,QC,6H,7H,6D] - Discard: 4
[KH,QC,6H,--,6D] - Discard: 2
[KH,--,6H,--,6D] - Discard:
[KH,6H,6D] + [3D,10S] = [KH,6H,6D,3D,10S]
group_hand -> [[6H,6D],[KH],[10S],[3D]]
You have: Just ("Two of a kind",[[6H,6D]])
Play again ([y], n)?
Cards in deck: 39
[KS,2S,10C,7D,JD] - Discard: 2
[KS,--,10C,7D,JD] - Discard: 4
[KS,--,10C,--,JD] - Discard:
[KS,10C,JD] + [3H,2D] = [KS,10C,JD,3H,2D]
group_hand -> [[KS],[JD],[10C],[3H],[2D]]
You have: Nothing
Play again ([y], n)?
```

