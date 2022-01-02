# Notes
Make the template x.go and test_x.go. Embed input.txt?
Keep some usual suspects input parsing in the aoc package (int/char matrix is one).

# 2021

## 12
Implemented recursive DFS, try BFS approach.

## 13 - Folding dots

## 14 - Expanding polymer pairs
Compact state using counts!

## 15 - Shortest path
Golang sorted heap.

## 16 - Bit interpreter
Try to operate on bits instead of []int ?

## 17 - Probe launcher
Brute force.

## 18 - Tree of ints
Try a binary tree approach
Something like github.com/bozdoz did:

```
type Element struct {
	value int
	pair  *Pair
}

type Pair struct {
	left, right *Element
	parent      *Pair
}
```

Or save just int and depth?
Explode just replaces two depth-four values with a depth-three 0.
Then Updates neighbours.
Split replaces one item with two.

## 19 - Beacon scans
Rotating (x,y,z) scans to see if they overlap between sample sets.

## 20 - Image enhancement
Infinitely growing grid of pixels.
Note that the background can light up every other step.

## 21 - Splitting universes when playing dice.
Part 1 can be brute forced.

Part 2 is trickier.
Each dice causes 3 realities, so each turn is 27 realities.
Make sure to properly propagate how many universes are created.

## 22 - Intersecting cubes
Part 1 brute forced with map[[3]int]bool .

Part 2 is too big.
Instead overlap cubes in a list.
Resize some cubes to match how they effect earlier ones.
Sometimes add new cubes with on/off to keep preserve total count.

## 23 - Move amphipods into their home
Graph search, tried to many implementations.
Unexpected differences in performance, could be interesting to profile?

In the end, memoizing state (via string) drastically shortened the solution time.

## 24 - ALU
Lots of pen and paper...

## 25 - Moving cucumbers
Saved state in a matrix, keep a list of moves to make and do them later to avoid double-move etc.
