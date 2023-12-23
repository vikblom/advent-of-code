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

type state struct {
	at   XY
	seen map[XY]bool
}

var nbr4delta = []aoc.XY{{X: -1, Y: 0}, {X: 1, Y: 0}, {X: 0, Y: -1}, {X: 0, Y: 1}}

func TestPartOne(t *testing.T) {
	m := aoc.ParseMatrix(input)

	start := XY{0, 1}
	goal := XY{m.Rows - 1, m.Cols - 2}

	ans := 0
	qs := []state{{start, map[XY]bool{}}}
	for len(qs) > 0 {
		q := qs[len(qs)-1]
		qs = qs[:len(qs)-1]

		if q.at == goal {
			ans = max(ans, len(q.seen))
		}

		s := maps.Clone(q.seen)
		s[q.at] = true

		switch m.At(q.at.X, q.at.Y) {
		case '>':
			n := XY{X: q.at.X, Y: q.at.Y + 1}
			if !q.seen[n] {
				qs = append(qs, state{
					at:   n,
					seen: s,
				})
			}
		case 'v':
			n := XY{X: q.at.X + 1, Y: q.at.Y}
			if !s[n] {
				qs = append(qs, state{
					at:   n,
					seen: s,
				})
			}
		default:
			for _, d := range nbr4delta {
				n := XY{X: q.at.X + d.X, Y: q.at.Y + d.Y}
				if !m.Inbounds(n.X, n.Y) || q.seen[n] || m.At(n.X, n.Y) == '#' {
					continue
				}
				qs = append(qs, state{
					at:   n,
					seen: s,
				})
			}
		}

	}

	aoc.Answer(t, ans, 2326)
}

type state2 struct {
	idx  int
	seen []bool
	dist int
}

func TestPartTwo(t *testing.T) {
	m := aoc.ParseMatrix(input)

	start := XY{0, 1}
	goal := XY{m.Rows - 1, m.Cols - 2}

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
					if reachable.At(i, j) != 0 && reachable.At(i, j) != dist {
						panic("assertion failed")
					}
					reachable.Set(i, j, dist)
					continue
				}

				for _, d := range nbr4delta {
					n := XY{X: at.X + d.X, Y: at.Y + d.Y}
					if !m.Inbounds(n.X, n.Y) || m.At(n.X, n.Y) == '#' {
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
	// dump(reachable)

	// DFS brute force going from intersection to intersection.
	ans := 0
	qs := []state2{{
		idxMap[start],
		make([]bool, len(intersects)),
		0,
	}}
	for len(qs) > 0 {
		q := qs[len(qs)-1]
		qs = qs[:len(qs)-1]

		if q.idx == idxMap[goal] {
			ans = max(ans, q.dist)
		}

		s := slices.Clone(q.seen)
		s[q.idx] = true

		for j := 0; j < reachable.Cols; j++ {
			toNext := reachable.At(q.idx, j)
			if toNext == 0 {
				continue // Not reachable
			}
			if q.seen[j] {
				continue
			}
			qs = append(qs, state2{
				idx:  j,
				seen: s,
				dist: q.dist + toNext,
			})
		}
	}

	aoc.Answer(t, ans, 6574)

}

func dump(m aoc.Matrix[int]) {
	for r := 0; r < m.Rows; r++ {
		for c := 0; c < m.Cols; c++ {
			fmt.Printf("%4d", m.At(r, c))
		}
		fmt.Println()
	}
}
