package solve

import (
	"testing"

	aoc "gitlab.com/vikblom/advent-of-code"

	_ "embed"
)

var (
	//go:embed "input.txt"
	input string
)

func shiftNorth(m aoc.Matrix[byte], r, c int) {
	if r == m.Rows-1 {
		return
	}
	for s := r + 1; s < m.Rows; s++ {
		switch m.At(s, c) {
		case 'O':
			m.Set(r, c, 'O')
			m.Set(s, c, '.')
			return
		case '#':
			return
		case '.':
			// continue
		}
	}
}

func shiftWest(m aoc.Matrix[byte], r, c int) {
	if c == m.Cols-1 {
		return
	}
	for s := c + 1; s < m.Cols; s++ {
		switch m.At(r, s) {
		case 'O':
			m.Set(r, c, 'O')
			m.Set(r, s, '.')
			return
		case '#':
			return
		case '.':
			// continue
		}
	}
}

func shiftSouth(m aoc.Matrix[byte], r, c int) {
	if r == 0 {
		return
	}
	for s := r - 1; s >= 0; s-- {
		switch m.At(s, c) {
		case 'O':
			m.Set(r, c, 'O')
			m.Set(s, c, '.')
			return
		case '#':
			return
		case '.':
			// continue
		}
	}
}

func shiftEast(m aoc.Matrix[byte], r, c int) {
	if c == 0 {
		return
	}
	for s := c - 1; s >= 0; s-- {
		switch m.At(r, s) {
		case 'O':
			m.Set(r, c, 'O')
			m.Set(r, s, '.')
			return
		case '#':
			return
		case '.':
			// continue
		}
	}
}

func TestPartOne(t *testing.T) {
	m := aoc.ParseMatrix(input)

	for r := 0; r < m.Rows-1; r++ {
		for c := 0; c < m.Cols; c++ {

			if m.At(r, c) == '.' {
				shiftNorth(m, r, c)
			}
		}
	}

	ans := 0
	for r := 0; r < m.Rows; r++ {
		for c := 0; c < m.Cols; c++ {
			if m.At(r, c) == 'O' {
				ans += m.Rows - r
			}
		}
	}
	aoc.Answer(t, ans, 107430)
}

func TestPartTwo(t *testing.T) {
	m := aoc.ParseMatrix(input)

	cycles := 1_000_000_000
	seen := map[string]int{}
	for n := 0; n < cycles; n++ {
		for dir := 0; dir < 4; dir++ {
			// The shifts assume we're iterating in a certain order.
			// If this isn't custom, then 'O's will block each other from moving.
			// To shift everything north, we start from the north and go south.
			switch dir {
			case 0:
				for r := 0; r < m.Rows-1; r++ {
					for c := 0; c < m.Cols; c++ {
						if m.At(r, c) != '.' {
							continue
						}
						shiftNorth(m, r, c)
					}
				}
			case 1:
				for r := 0; r < m.Rows; r++ {
					for c := 0; c < m.Cols-1; c++ {
						if m.At(r, c) != '.' {
							continue
						}
						shiftWest(m, r, c)
					}
				}
			case 2:
				for r := m.Rows - 1; r >= 0; r-- {
					for c := 0; c < m.Cols; c++ {
						if m.At(r, c) != '.' {
							continue
						}
						shiftSouth(m, r, c)
					}
				}
			case 3:
				for r := 0; r < m.Rows; r++ {
					for c := m.Cols - 1; c >= 0; c-- {
						if m.At(r, c) != '.' {
							continue
						}
						shiftEast(m, r, c)
					}
				}
			}
		}

		if prev, ok := seen[m.String()]; ok {
			// We've been here before, so short circuit the loop by
			// subtracting however many whole periods we can
			// fit in whatever is left.
			cycles = n + (cycles-prev)%(n-prev)
		} else {
			seen[m.String()] = n
		}
	}

	ans := 0
	for r := 0; r < m.Rows; r++ {
		for c := 0; c < m.Cols; c++ {
			if m.At(r, c) == 'O' {
				ans += m.Rows - r
			}
		}
	}
	aoc.Answer(t, ans, 96317)
}
