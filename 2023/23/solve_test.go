package solve

import (
	"fmt"
	"maps"
	"slices"
	"strings"
	"testing"

	aoc "gitlab.com/vikblom/advent-of-code"

	_ "embed"
)

type XY aoc.XY

var (
	//go:embed "input.txt"
	input      string
	inputLines = strings.Split(strings.TrimRight(input, "\n"), "\n")
)

var nbr4delta = []aoc.XY{{X: -1, Y: 0}, {X: 1, Y: 0}, {X: 0, Y: -1}, {X: 0, Y: 1}}

func nbrs(m aoc.Matrix[byte], x, y int) []XY {
	switch m.At(x, y) {
	case '>':
		return []XY{{X: x, Y: y + 1}}
	case 'v':
		return []XY{{X: x + 1, Y: y}}
	default:
		out := []XY{}
		for _, d := range nbr4delta {
			out = append(out, XY{X: x + d.X, Y: y + d.Y})
		}
		return out
	}
}

type DFS struct {
	ans  int
	m    aoc.Matrix[byte]
	seen aoc.Matrix[bool]
}

func (d *DFS) recur(x, y, dist int) {
	if x == d.m.Rows-1 && y == d.m.Cols-2 {
		d.ans = max(d.ans, dist)
		return
	}

	for _, n := range nbrs(d.m, x, y) {
		if !d.m.Inbounds(n.X, n.Y) || d.seen.At(n.X, n.Y) || d.m.At(n.X, n.Y) == '#' {
			continue
		}
		d.seen.Set(n.X, n.Y, true)
		d.recur(n.X, n.Y, dist+1)
		d.seen.Set(n.X, n.Y, false)
	}
}

func TestPartOne(t *testing.T) {
	m := aoc.ParseMatrix(input)
	seen := aoc.NewMatrix[bool](m.Rows, m.Cols)

	dfs := &DFS{m: m, seen: seen}
	dfs.recur(0, 1, 0)
	aoc.Answer(t, dfs.ans, 2326)
}

func TestPartTwo(t *testing.T) {
	m := aoc.ParseMatrix(input)

	start := XY{0, 1}
	goal := XY{m.Rows - 1, m.Cols - 2}

	intersects, reachable := reachability(m, start, goal)
	// dump(reachable)

	dfs := &DFS2{
		reachable: reachable,
		seen:      make([]bool, len(intersects)),
		goal:      slices.Index(intersects, goal),
	}
	dfs.recur(slices.Index(intersects, start), 0)

	aoc.Answer(t, dfs.ans, 6574)
}

func TestAlt(t *testing.T) {
	m := aoc.ParseMatrix(input)

	start := XY{0, 1}
	goal := XY{m.Rows - 1, m.Cols - 2}

	intersects, reachable := reachability(m, start, goal)
	fmt.Println(intersects)
	dump(reachable)

	si := slices.Index(intersects, start)
	gi := slices.Index(intersects, goal)
	qs := []state{{
		i:    si,
		seen: 1 << si,
		dist: 0,
	}}

	ans := 0
	for len(qs) > 0 {
		q := qs[len(qs)-1]
		qs = qs[:len(qs)-1]

		if q.i == gi {
			ans = max(ans, q.dist)
			continue
		}

		for j := 0; j < reachable.Cols; j++ {
			toNext := reachable.At(q.i, j)
			if toNext == 0 {
				continue // Not reachable
			}
			if q.seen&(1<<j) > 0 {
				continue
			}
			qs = append(qs, state{
				i:    j,
				seen: q.seen | (1 << j),
				dist: q.dist + toNext,
			})
		}
	}

	aoc.Answer(t, ans, 6574)
}

type state struct {
	i    int
	dist int
	seen int
}

type DFS2 struct {
	ans       int
	reachable aoc.Matrix[int]
	seen      []bool
	goal      int
}

func (d *DFS2) recur(i, dist int) {
	if i == d.goal {
		d.ans = max(d.ans, dist)
		return
	}

	for j := 0; j < d.reachable.Cols; j++ {
		toNext := d.reachable.At(i, j)
		if toNext == 0 {
			continue // Not reachable
		}
		if d.seen[j] {
			continue
		}
		d.seen[j] = true
		d.recur(j, dist+toNext)
		d.seen[j] = false
	}
}

func reachability(m aoc.Matrix[byte], start, goal XY) ([]XY, aoc.Matrix[int]) {
	// Find all intersections.
	intersects := []XY{start, goal}
	for r := 1; r < m.Rows-1; r++ {
		for c := 1; c < m.Cols-1; c++ {
			if m.At(r, c) == '#' {
				continue
			}
			open := 0
			for _, d := range nbr4delta {
				n := XY{X: r + d.X, Y: c + d.Y}
				if m.At(n.X, n.Y) != '#' {
					open += 1
				}
			}
			if open > 2 {
				intersects = append(intersects, XY{r, c})
			}
		}
	}
	idxMap := map[XY]int{}
	for i, ix := range intersects {
		idxMap[ix] = i
	}

	// Flood fill to find how fast we can reach each connect intersection.
	reachable := aoc.NewMatrix[int](len(intersects), len(intersects))
	for i, from := range intersects {
		filled := map[XY]int{from: 0}
		for {
			fill := map[XY]int{}
			for at, dist := range filled {
				if j, ok := idxMap[at]; ok && i != j {
					// All paths between two intersections are equally long.
					delete(filled, at)
					reachable.Set(i, j, dist)
					continue
				}

				for _, d := range nbr4delta {
					n := XY{X: at.X + d.X, Y: at.Y + d.Y}
					if !m.Inbounds(n.X, n.Y) || m.At(n.X, n.Y) == '#' {
						continue
					}
					if j, ok := idxMap[at]; ok && i != j {
						reachable.Set(i, j, dist)
						continue
					}
					if _, ok := filled[n]; ok {
						continue
					}
					fill[n] = dist + 1
				}
			}
			maps.Copy(filled, fill)
			if len(fill) == 0 {
				break
			}
		}
	}

	return intersects, reachable
}

func dump(m aoc.Matrix[int]) {
	fmt.Println("states:", m.Rows)
	for r := 0; r < m.Rows; r++ {
		for c := 0; c < m.Cols; c++ {
			fmt.Printf("%4d", m.At(r, c))
		}
		fmt.Println()
	}
}
