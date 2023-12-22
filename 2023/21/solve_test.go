package solve

import (
	"fmt"
	"maps"
	"math"
	"testing"

	aoc "gitlab.com/vikblom/advent-of-code"

	_ "embed"
)

var (
	//go:embed "input.txt"
	input string
)

type XY aoc.XY

// hash crimes in the name of speed...
func (xy *XY) hash() int {
	return (xy.X+math.MaxUint16)<<32 + (xy.Y + math.MaxUint16)
}

func unhash(h int) XY {
	return XY{
		X: (h >> 32) - math.MaxUint16,
		Y: h&(1<<32-1) - math.MaxUint16,
	}
}

func nbrs4(at XY) []XY {
	return []XY{
		{at.X + 1, at.Y},
		{at.X - 1, at.Y},
		{at.X, at.Y + 1},
		{at.X, at.Y - 1},
	}
}

func wrap(i, m int) int {
	if i < 0 {
		i += m
		return (i + (-i/m+1)*m) % m
	}
	return i % m
}

func TestPartOne(t *testing.T) {
	m := aoc.ParseMatrix(input)
	var start XY
	for r := 0; r < m.Rows; r++ {
		for c := 0; c < m.Cols; c++ {
			if m.At(r, c) == 'S' {
				start = XY{r, c}
				m.Set(r, c, '.')
			}
		}
	}

	filled := map[XY]bool{start: true}

	for i := 0; i < 64; i++ {
		fill := map[XY]bool{}
		for at := range filled {
			for _, n := range nbrs4(at) {
				if m.At(n.X, n.Y) == '.' {
					fill[n] = true
				}
			}
		}
		filled = fill
	}

	aoc.Answer(t, len(filled), 3748)
}

func brute(m aoc.Matrix[byte], start XY, steps int) int {
	i := 0
	dist := map[int]int{start.hash(): i}
	prev2 := dist
	for {
		fill := map[int]int{}
		for h := range prev2 {
			at := unhash(h)

			for _, n := range nbrs4(at) {
				x := wrap(n.X, m.Rows)
				y := wrap(n.Y, m.Cols)

				_, seen := dist[n.hash()]
				if !seen && m.At(x, y) == '.' {
					fill[n.hash()] = (i + 1)
				}
			}
		}
		maps.Copy(dist, fill)
		prev2 = fill
		i++
		if i > steps {
			break
		}
	}

	ans := 0
	for _, v := range dist {
		if v%2 == steps%2 {
			ans += 1
		}
	}
	return ans
}

func TestPartTwo(t *testing.T) {
	m := aoc.ParseMatrix(input)
	var start XY
	for r := 0; r < m.Rows; r++ {
		for c := 0; c < m.Cols; c++ {
			if m.At(r, c) == 'S' {
				start = XY{r, c}
				m.Set(r, c, '.')
			}
		}
	}

	var steps, ans, delta, accel int
	var prevAns, prevDelta int
	for s := 0; s < 16; s++ {
		steps = m.Rows/2 + s*m.Rows
		if steps%2 == 0 {
			continue
		}

		ans = brute(m, start, steps)
		delta = ans - prevAns
		shouldStop := accel == (delta - prevDelta)
		accel = delta - prevDelta

		fmt.Println(delta, accel)

		prevAns = ans
		prevDelta = delta
		if shouldStop {
			break
		}
	}

	for at := steps; at < 26501365; at += 2 * m.Rows {
		delta += accel
		ans += delta
	}
	aoc.Answer(t, ans, 616951804315987)
}
