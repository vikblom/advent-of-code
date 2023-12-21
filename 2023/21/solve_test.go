package solve

import (
	"fmt"
	"maps"
	"testing"

	aoc "gitlab.com/vikblom/advent-of-code"

	_ "embed"
)

type XY aoc.XY

var (
	//go:embed "input.txt"
	input string
)

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
	dist := map[XY]int{start: i}
	prev2 := dist
	for {
		fill := map[XY]int{}
		for at := range prev2 {
			for _, n := range nbrs4(at) {
				x := wrap(n.X, m.Rows)
				y := wrap(n.Y, m.Cols)

				_, seen := dist[n]
				if !seen && m.At(x, y) == '.' {
					fill[n] = (i + 1)
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

var (
	//go:embed example.txt
	example string
)

func TestBrute(t *testing.T) {
	m := aoc.ParseMatrix(example)
	var start XY
	for r := 0; r < m.Rows; r++ {
		for c := 0; c < m.Cols; c++ {
			if m.At(r, c) == 'S' {
				start = XY{r, c}
				m.Set(r, c, '.')
			}
		}
	}

	for _, s := range []int{6, 10, 50, 100, 500 /*, 1000, 5000*/} {
		t.Log("brute", s, brute(m, start, s))
	}
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
