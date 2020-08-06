# Logical Guessing Game 

This haskell program implements a game somewhatakin to the game of BattleShip.
The hider starts the game by selecting a three target location coordinate, where each location coordinate comprise a
colume and a row. For example, ("A1","B2","C3").
The searcher repeatedly choose a list of three location coordinate as a guess and give
it to the hider. The hider then responds the performer by giving
a feedback. This game is finished when the searcher find exactly the location coordinate that hider choose.
The goal of this program is to minimize the number of attempts.

