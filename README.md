# bfh
bfh is an optimising interpreter for the brainf*** language written using haskell. 
https://esolangs.org/wiki/Brainfuck

bfh.hs can be compiled to a binary by calling ghc on it directly.

To reduce portability issues (and for fun) many of the tapes properties can be customized through command line arguments.
The first argument should be the location of a brainf*** source file.
The second argument must be an integer specifying the size of the cells.
8, 16, 32, and 64 will run the program with corresponding n-bit cells with proper over/underflowing. 
Entering 0 will run the program with unbounded cells which do not over/underflow.
All other integer arguments will result in 8-bit cell size.

After the filepath and cell size, optional arguments can be entered in any order.
The default tape starts on the left end and has an unbounded right side.

The arguments -l and -r allow specified lengths for the left and right sides of the initial cell.
These should be followed by a space and an integer argument.
Passing "-r 30000 -l 30000" will result in a tape with 30000 cells on both sides of the first cell
for a total of 60001 cells. A negative integer will create a tape with unbounded length on that side.
"-r (-1) -l (-67)" will create a tape with infinite cells in both directions.

Using -w will enable wrapping on finite tapes. If either end of the tape is unbounded then this
argument is ignored. With this argument, moving off the end of a finite tape will place the
cell pointer at the opposite end of the tape. Without wrapping, moving off the end will cause
the cell pointer to remain just off the tape until the first instruction which moves it back on the
tape is encountered. If an increment, decrement, input, or output command is run while off the tape,
then the program will crash.

Using -i followed by an integer, the initial value of each cell can be specified. Normally each cell 
starts as 0, but "-i 1" will cause each cell to start with a value of 1. For finite cells, passing an
integer n outside of the cell range will set each cells value to n modulo m where m is the maximum value
that can be contained in a cell. The arguments "lol.b 8 -i 257" and "lol.b 8 -i (-255)" will both result
in all 8 bit cells being initialized to 1. Passing a negative value to unbounded cells will result in
undefined behavior.
