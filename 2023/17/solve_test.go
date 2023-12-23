package solve

import (
	"strings"
	"testing"

	"github.com/google/btree"
	aoc "gitlab.com/vikblom/advent-of-code"

	_ "embed"
)

// Optimized solution
//
// Replaced container/heap with btree.
// The generic api avoids some interface overhead.
//
// Avoids returning candidate "neighbours" from a function
// since that allocated too much, even if the slice for it
// is reusing backing storage. Instead construct
// candidate in-place.
//
// Use a custom hash-function for state so it
// hits the fast64 part of builtin maps.

var (
	//go:embed "input.txt"
	input      string
	inputLines = strings.Split(strings.TrimRight(input, "\n"), "\n")
)

// State in our graph traversal.
// Since we can't reverse, and can't keep going in the same
// direction, keep track of if we're moving horizontally or
// vertically.
//
// Note that going down on a line, three steps right, then down again:
//
//	v>>>
//	   v
//
// is ok, even if it technically is 4 in a row.
type state struct {
	pos        aoc.XY
	horizontal bool
	score      int
}

func (s *state) hash() int {
	u := s.pos.X + s.pos.Y<<8
	if s.horizontal {
		u += 1 << 16
	}
	return u
}

var (
	vertDelta = []aoc.XY{
		{X: -1, Y: 0}, {X: -2, Y: 0}, {X: -3, Y: 0},
		{X: 1, Y: 0}, {X: 2, Y: 0}, {X: 3, Y: 0},
	}
	horzDelta = []aoc.XY{
		{X: 0, Y: -1}, {X: 0, Y: -2}, {X: 0, Y: -3},
		{X: 0, Y: 1}, {X: 0, Y: 2}, {X: 0, Y: 3},
	}
)

func TestPartOne(t *testing.T) {
	raw := aoc.ParseMatrix(input)
	ints := []int{}
	for _, b := range raw.Slice() {
		ints = append(ints, aoc.MustInt(string(b)))
	}
	m := aoc.ToMatrix(ints, raw.Rows, raw.Cols)
	goal := aoc.XY{X: m.Rows - 1, Y: m.Cols - 1}

	q := btree.NewG(8, func(a, b state) bool {
		// Cheat the total order since we don't want
		// btree to de-duplicate states with the same score.
		return a.score <= b.score
	})
	// Start is all zero.
	q.ReplaceOrInsert(state{horizontal: false})
	q.ReplaceOrInsert(state{horizontal: true})
	best := map[int]int{0: 0}

	var s state
	for q.Len() > 0 {
		s, _ = q.DeleteMin()
		if s.pos == goal {
			break
		}

		var nbrs []aoc.XY
		if s.horizontal {
			nbrs = vertDelta
		} else {
			nbrs = horzDelta
		}

		cost := 0
		for i, d := range nbrs {
			n := aoc.XY{X: s.pos.X + d.X, Y: s.pos.Y + d.Y}
			if !m.Inbounds(n.X, n.Y) {
				continue
			}
			if i%3 == 0 {
				cost = 0
			}
			cost += m.At(n.X, n.Y)

			next := state{
				pos:        n,
				horizontal: !s.horizontal,
				score:      s.score + cost,
			}
			prev := best[next.hash()]
			if 0 < prev && prev <= next.score {
				continue
			}
			best[next.hash()] = next.score
			q.ReplaceOrInsert(next)
		}
	}

	aoc.Answer(t, s.score, 698)
}

type nbr struct {
	pos  aoc.XY
	cost int
}

var (
	vert = []aoc.XY{
		{X: -1, Y: 0},
		{X: 1, Y: 0},
	}
	horz = []aoc.XY{
		{X: 0, Y: -1},
		{X: 0, Y: 1},
	}
)

func TestPartTwo(t *testing.T) {
	raw := aoc.ParseMatrix(input)
	ints := []int{}
	for _, b := range raw.Slice() {
		ints = append(ints, aoc.MustInt(string(b)))
	}
	m := aoc.ToMatrix(ints, raw.Rows, raw.Cols)
	goal := aoc.XY{X: m.Rows - 1, Y: m.Cols - 1}

	q := btree.NewG(16, func(a, b state) bool {
		// Cheat the total order since we don't want
		// btree to de-duplicate states with the same score.
		return a.score <= b.score
	})
	// Start is all zero.
	q.ReplaceOrInsert(state{horizontal: false})
	q.ReplaceOrInsert(state{horizontal: true})
	best := map[int]int{0: 0}

	var s state
	for q.Len() > 0 {
		s, _ = q.DeleteMin()

		if s.pos == goal {
			break
		}

		var dirs []aoc.XY
		if s.horizontal {
			dirs = vert
		} else {
			dirs = horz
		}
		if s.pos.X == 0 && s.pos.Y == 0 {
			dirs = append(vert, horz...)
		}

	Dir:
		for _, dir := range dirs {
			if !m.Inbounds(s.pos.X+4*dir.X, s.pos.Y+4*dir.Y) {
				continue
			}
			cost := 0
			x := s.pos.X
			y := s.pos.Y
			for i := 1; i < 4; i++ {
				x += dir.X
				y += dir.Y
				if !m.Inbounds(x, y) {
					continue Dir
				}
				cost += m.At(x, y)
			}
			for i := 4; i <= 10; i++ {
				x += dir.X
				y += dir.Y
				if !m.Inbounds(x, y) {
					continue Dir
				}
				cost += m.At(x, y)

				next := state{
					pos:        aoc.XY{X: x, Y: y},
					horizontal: !s.horizontal,
					score:      s.score + cost,
				}

				prev := best[next.hash()]
				if 0 < prev && prev <= next.score {
					continue
				}
				best[next.hash()] = next.score
				q.ReplaceOrInsert(next)
			}
		}

	}

	aoc.Answer(t, s.score, 825)
}
