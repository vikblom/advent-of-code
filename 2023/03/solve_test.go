package solve

import (
	"bytes"
	"testing"

	_ "embed"

	aoc "gitlab.com/vikblom/advent-of-code"
)

//go:embed "input.txt"
var raw []byte

func isDigit(b byte) bool {
	return '0' <= b && b <= '9'
}

func TestPartOne(t *testing.T) {
	cols := bytes.Index(raw, []byte{'\n'})
	data := bytes.ReplaceAll(raw, []byte{'\n'}, nil)
	m := aoc.ToMatrix(data, len(data)/cols, cols)

	sum := 0
	for row := 0; row < m.Rows; row++ {
		digits := []byte{}
		partNum := false
		for col := 0; col < m.Cols; col++ {
			at := m.At(row, col)
			if isDigit(at) {
				digits = append(digits, at)

				for _, n := range m.Nbrs8(row, col) {
					nn := m.At(n.X, n.Y)
					if nn != '.' && !isDigit(nn) {
						partNum = true
					}
				}
			}

			if !isDigit(at) || col == m.Cols-1 {
				if partNum {
					n := aoc.MustInt(string(digits))
					sum += n
				}
				digits = []byte{}
				partNum = false
			}
		}
	}

	aoc.Answer(sum, 537832)
}

func TestPartTwo(t *testing.T) {
	cols := bytes.Index(raw, []byte{'\n'})
	data := bytes.ReplaceAll(raw, []byte{'\n'}, nil)
	m := aoc.ToMatrix(data, len(data)/cols, cols)

	// Accumulate each number next to a '*'.
	asterisks := map[aoc.XY][]int{}

	for row := 0; row < m.Rows; row++ {
		// Finish the number first, then check if any of the
		// nearby slots had a '*'.
		digits := []byte{}
		nbrs := map[aoc.XY]struct{}{}

		for col := 0; col < m.Cols; col++ {
			at := m.At(row, col)
			if isDigit(at) {
				digits = append(digits, at)
				for _, n := range m.Nbrs8(row, col) {
					nbrs[n] = struct{}{}
				}
			}

			if !isDigit(at) || col == m.Cols-1 {
				for n := range nbrs {
					at := m.At(n.X, n.Y)
					if at == '*' {
						ratio := aoc.MustInt(string(digits))
						asterisks[n] = append(asterisks[n], ratio)
					}
				}

				digits = []byte{}
				nbrs = map[aoc.XY]struct{}{}
			}
		}
	}

	sum := 0
	for _, v := range asterisks {
		if len(v) == 2 {
			sum += v[0] * v[1]
		}
	}
	aoc.Answer(sum, 81939900)
}
