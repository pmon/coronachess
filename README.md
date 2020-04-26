# coronachess
My chess engine exercise in scheme lang

CoronaChess is an exercise project to learn and use the scheme language. It is a fully functional chess engine supporting UCI protocol and implementing the following chess engine techniques
* Magic bitboards
* Legal move generation
* Zobrist hashing
* Transposition tables
* Move ordering
* Static Exchange Evaluation
* Piece square tables
* Late move reduction
* Null move pruning
* Quiesce search
* Principal Variation search
* Iterative deepening
* Time management

CoronaChess has been developed with Gambit-C scheme dialect, it runs in the scheme interpreter as weel as a native executable after compilation.
To compile CoronaChess with the Gambit-C compiler, run the following command into a terminal window
```
gsc -exe coronachess.scm
```
then run coronachess executable or connect an UCI compatible chess interface to the coronachess executable.

CoronaChess is very slow compared to other chess engines but it still gives me hard and fun blitz matches, hope you can enjoy it also ;-)

