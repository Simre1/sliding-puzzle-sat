# sliding-puzzle-sat

Haskell program which calculates the sat formula for a 3x3 sliding puzzle game which can be solved with limboole.

## The Game

You start with a 3x3 field of numbers from 0 to 8 where each number occurs once:

```
4 3 5
9 2 3
1 0 7
```

You may swap numbers with the `0` vertically and horizontically. You need to rearrange the numbers in an ascending manner:

```
1 2 3
4 5 6
7 8 0
```

## How to run

You need the Haskell build tool `cabal` and the Haskell compiler `ghc` (use ghcup to install them). Then you can run the program with:

```
cabal run sat-game <number_of_steps> <field1> <field2> ... <field9>
```

`<number_of_steps>` tells the sat solver how many moves to try to find a solution.

`<fieldX>` tells the sat solver where the numbers are in the initial state.

The field arguments are arranged in this manner:
```
<field1> <field2> <field3>
<field4> <field5> <field6>
<field7> <field8> <field9>
```
