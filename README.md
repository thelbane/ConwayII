# Conway's Game of Life for Apple II
Written in 6502 assembly and not documented very well.

![Screenshot](screenshots/capture.gif)

## Building the Project
### Give the Makefile a Spin
Who knows? Maybe you're super nerdy and have all the prerequisites to run the `makefile`.

Type:
```sh
git clone git@github.com:thelbane/ConwayII.git
cd ConwayII
make run
```
(If this worked for you on the first try, you're officially a bigger loser than me.)

### Makefile Requirements
Here's a list of the prerequisites you'll need to make the `makefile` work.

* [DASM](https://sourceforge.net/projects/dasm-dillon/files/dasm-dillon/) (2.20.11) - Macro assembler.
  * Make sure this is saved somewhere in your PATH.
* [AppleCommander](https://sourceforge.net/projects/applecommander/files/AppleCommander%20-%20Stable/) (1.3.5) - Apple II disk image utility.
  * Requires Java (because this will be neither easy nor straightforward)
  * Save the .jar file to `/usr/local/bin/ac.jar` or modify the path in the `makefile` using a text editor
* [Virtual \]\[](http://www.virtualii.com/) (7.6) - Apple II emulator.
  * Requires Apple II ROMs (because emulation)

Once you've tried and failed to satisfy all of these requirements (because you realized too late that Virtual ][ is a Mac emulator and you're on Windows), just launch your favorite Apple II emulator and open `diskimages/conway.dsk`.

### I Just Want to Assemble the Damn Thing
If you have DASM installed, assemble the project with:

```sh
dasm src/conway.asm -oconway -v4 -f2
```

As if by magic, you'll see the following file has been created:
```sh
-rw-r--r--   1 thelbane  staff   1.1K May  4 21:29 conway
```

Now type `./conway` and watch your so-called "modern" PC utterly fail to comprehend what to do with this 1,080 byte program. This is where a disk image utility (like AppleCommander) comes in handy.

## It Doesn't Really Seem That Fast
So you installed Java (twice) and found three different //e ROMs that are all slightly different, but all sort of work... and got it up and running... or you chose to just stare at the animated gif above and get all of the reward without any of the effort. And you're thinking, "He calls this 'pretty fast?' I've implemented Game of Life in Javascript, which is like today's version of BASIC, and it ran about a gajillion times faster than this."

Well, I stand by my claim that it's pretty fast... for an Apple II running at 1MHz.

### Optimizations
I'll admit the source is not organized particularly well and the comments are rather uninspired, but if you're comfortable with 6502 assembly, it shouldn't be too difficult to follow.

Still, here's a list of the optimizations I've made. (Probably not exhaustive.)
* Padded data
  * The matrix of visible cells is surrounded by an invisible layer of hidden cells. e.g., There are 40x24 visible cells, but the array containing information about the cells is 42x26.
  * This removes the need for bounds checking when evaluating neighboring cells in the innermost loop, resulting in a significant speedup.
  * This is a pretty common optimization since it's easy to reason about and implement.
* Lookup table for rules
  * Given that every cell has 8 neighbors that are either on or off, it made sense to treat those states as bit flags stored in a single byte.
  * The bits are arranged as such (the numbers represent bit position from the left):
```
 0 1 2     0 = top left neighbor      bitmask: 10000000
 3   4     1 = top neighbor           bitmask: 01000000
 5 6 7     2 = top right neighbor     bitmask: 00100000
           3 = left neighbor          bitmask: 00010000
           4 = right neighbor         bitmask: 00001000
           ... etc.
```
  * Example: If a cell's bottom three neighbors are turned on, that would be represented with byte %00000111 or $07. And we would find at the 7th index of our lookup table a rule indicating that the cell should be ON since any cell with three neighbors enabled will either stay or turn on (as if by reproduction). In fact, at every index that can be represented by a bit pattern with three bits enabled you'll find an ON rule. Likewise, at index 0 (no enabled bits), 1 (one enabled bit %00000001), 2 (one enabled bit %00000010), 4, 8, 16, 32, 64, and 128, you'll find OFF rules because cells with fewer than 3 neighbors die off (as if by loneliness).
  * Cells with two neighbors remain unchanged. Cells with greater than 3 neighbors die off (as if by overcrowding).
* Draw and update in the same loop
  * While there's only one visible screen of data, the Conway rules must be applied atomically (all at once), necessitating two pages worth of cell data (one representing the current generation [mainData], and another representing the next generation [altData]).
  * The pointers for mainData and altData swap before each full screen draw cycle.
  * Starting from the bottom right-most cell we work our way left and up the screen, one row at a time.
  * For each cell:
    * Use the byte in mainData to lookup the rule in the rules table and display a character
    * If the cell is ON, then update the *neighboring cells* in altData. This is where it gets weird. When we're on cell (x,y), we never actually modify the value of (x,y), we modify (x-1,y-1), (x,y-1), (x+1, y-1), etc. and enable the bits corresponding to the neighbor's relationship to (x,y). e.g., We enable the bottom right (%00000001) bit for our top left neighbor. (It doesn't strictly matter that we set the bit corresponding to the inverse relationship, as long as we're consistent. We only really care about the bit count and not their positions in the resulting byte.)
* Lookup table for screen addresses
  * The Apple II is weird in that text and graphics data is not stored from left to right, top to bottom, in a contiguous chunk of memory. It's stored all haphazard and non-sensical for reasons that others have pontificated on at length.
  * Needless to say, there's math to compute the addresses of a given point on the screen, but math is slow and lookup tables are silly fast.
  
I didn't do much with loop unrolling, but there are certainly opportunities to be gained there along with other optimizations. There's some housekeeping that has to be done between screen updates due to the way I'm clearing stale data during the main update loops.

Anyway, I hope this was at least mildly interesting to you. Cheers. :)
