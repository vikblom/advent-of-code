package solve

import (
	"strings"
	"testing"

	aoc "gitlab.com/vikblom/advent-of-code"

	_ "embed"
)

var (
	//go:embed "input.txt"
	input      string
	inputLines = strings.Split(strings.TrimRight(input, "\n"), "\n")
)

// Need the tail to see if we can continue stragiht or not.
// Going down on a line, three steps right, then down again:
//
//	v>>>
//	   v
//
// is ok, even if it technically is 4 in a row.
//
// Keep the accumulated cost outside the state so
// state can be used in hashmaps.
type State1 [4]aoc.XY

func Dist(a, b aoc.XY) int {
	return max(aoc.AbsInt(a.X-b.X), aoc.AbsInt(a.Y-b.Y))
}

func TestPartOne(t *testing.T) {
	raw := aoc.ParseMatrix(input)
	ints := []int{}
	for _, b := range raw.Slice() {
		ints = append(ints, aoc.MustInt(string(b)))
	}
	m := aoc.ToMatrix(ints, raw.Rows, raw.Cols)
	goal := aoc.XY{X: m.Rows - 1, Y: m.Cols - 1}

	q := aoc.NewQueue[State1]()
	// Start is all zero.
	q.Push(State1{}, 0)
	best := map[State1]int{{}: 0}

	var pos State1
	var cost int
	for q.Len() > 0 {
		pos, cost = q.Pop()
		if pos[0] == goal {
			break
		}

		for _, n := range m.Neighbours(pos[0].X, pos[0].Y) {
			if n == pos[1] {
				continue // Don't reverse.
			}
			if Dist(n, pos[3]) >= 4 {
				continue // Don't go too far.
			}

			next := State1{
				n,
				pos[0],
				pos[1],
				pos[2],
			}
			nextCost := cost + m.At(n.X, n.Y)
			prev := best[next]
			if 0 < prev && prev <= nextCost {
				continue
			}
			best[next] = nextCost
			q.Push(next, nextCost)
		}
	}

	aoc.Answer(t, cost, 698)
}

type nbr struct {
	pos  aoc.XY
	cost int
}

func nbrs(m aoc.Matrix[int], at, prev aoc.XY) []nbr {
	nbrs := []nbr{}
	// South
	if prev.X == at.X {
		if (at.X + 4) < m.Rows {
			c := 0
			for i := 1; i < 4; i++ {
				c += m.At(at.X+i, at.Y)
			}
			for x := at.X + 4; x < min(at.X+11, m.Rows); x++ {
				c += m.At(x, at.Y)
				nbrs = append(nbrs, nbr{pos: aoc.XY{X: x, Y: at.Y}, cost: c})
			}
		}
		// North
		if (at.X - 4) >= 0 {
			c := 0
			for i := 1; i < 4; i++ {
				c += m.At(at.X-i, at.Y)
			}
			for x := at.X - 4; x >= max(0, at.X-10); x-- {
				c += m.At(x, at.Y)
				nbrs = append(nbrs, nbr{pos: aoc.XY{X: x, Y: at.Y}, cost: c})
			}
		}
	}
	if prev.Y == at.Y {
		// East
		if (at.Y + 4) < m.Cols {
			c := 0
			for i := 1; i < 4; i++ {
				c += m.At(at.X, at.Y+i)
			}
			for y := at.Y + 4; y < min(at.Y+11, m.Cols); y++ {
				c += m.At(at.X, y)
				nbrs = append(nbrs, nbr{pos: aoc.XY{X: at.X, Y: y}, cost: c})
			}
		}
		// West
		if (at.Y - 4) >= 0 {
			c := 0
			for i := 1; i < 4; i++ {
				c += m.At(at.X, at.Y-i)
			}
			for y := at.Y - 4; y >= max(0, at.Y-10); y-- {
				c += m.At(at.X, y)
				nbrs = append(nbrs, nbr{pos: aoc.XY{X: at.X, Y: y}, cost: c})
			}
		}
	}

	return nbrs
}

type state2 struct {
	pos  aoc.XY
	prev aoc.XY
}

func TestPartTwo(t *testing.T) {
	raw := aoc.ParseMatrix(input)
	ints := []int{}
	for _, b := range raw.Slice() {
		ints = append(ints, aoc.MustInt(string(b)))
	}
	m := aoc.ToMatrix(ints, raw.Rows, raw.Cols)
	goal := aoc.XY{X: m.Rows - 1, Y: m.Cols - 1}

	q := aoc.NewQueue[state2]()
	// Start is all zero.
	q.Push(state2{}, 0)
	best := map[state2]int{{}: 0}

	var cost int
	var at state2
	for q.Len() > 0 {
		at, cost = q.Pop()

		if at.pos == goal {
			break
		}

		for _, n := range nbrs(m, at.pos, at.prev) {
			next := state2{
				pos:  n.pos,
				prev: at.pos,
			}
			nextCost := cost + n.cost

			prev := best[next]
			if 0 < prev && prev <= nextCost {
				continue
			}
			best[next] = nextCost
			q.Push(next, nextCost)
		}
	}

	aoc.Answer(t, cost, 825)
}
