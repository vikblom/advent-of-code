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
	inputBlobs = strings.Split(strings.TrimRight(input, "\n"), "\n\n")
)

type Matrix = aoc.Matrix[byte]

func rowsMirror(m Matrix, a, b int) bool {
	for ; 0 <= a && b < m.Rows; a, b = a-1, b+1 {
		for c := 0; c < m.Cols; c++ {
			if m.At(a, c) != m.At(b, c) {
				return false
			}
		}
	}
	return true
}

func colsMirror(m Matrix, a, b int) bool {
	for ; 0 <= a && b < m.Cols; a, b = a-1, b+1 {
		for r := 0; r < m.Rows; r++ {
			if m.At(r, a) != m.At(r, b) {
				return false
			}
		}
	}
	return true
}

func TestPartOne(t *testing.T) {
	ans := 0
	for _, b := range inputBlobs {
		m := aoc.ParseMatrix(b)

		for r := 1; r < m.Rows; r++ {
			if rowsMirror(m, r-1, r) {
				ans += 100 * r
			}
		}

		for c := 1; c < m.Cols; c++ {
			if colsMirror(m, c-1, c) {
				ans += c
			}
		}
	}
	aoc.Answer(t, ans, 33735)
}

func rowsSmudge(m Matrix, a, b int) bool {
	var smudged bool
	for ; 0 <= a && b < m.Rows; a, b = a-1, b+1 {
		for c := 0; c < m.Cols; c++ {
			if m.At(a, c) != m.At(b, c) {
				if !smudged {
					smudged = true
				} else {
					return false
				}
			}
		}
	}
	return smudged
}

func colsSmudge(m Matrix, a, b int) bool {
	var smudged bool
	for ; 0 <= a && b < m.Cols; a, b = a-1, b+1 {
		for r := 0; r < m.Rows; r++ {
			if m.At(r, a) != m.At(r, b) {
				if !smudged {
					smudged = true
				} else {
					return false
				}
			}
		}
	}
	return smudged
}

func TestPartTwo(t *testing.T) {
	ans := 0
	for _, b := range inputBlobs {
		m := aoc.ParseMatrix(b)

		for r := 1; r < m.Rows; r++ {
			if !rowsMirror(m, r-1, r) && rowsSmudge(m, r-1, r) {
				ans += 100 * r
			}
		}

		for c := 1; c < m.Cols; c++ {
			if !colsMirror(m, c-1, c) && colsSmudge(m, c-1, c) {
				ans += c
			}
		}
	}
	aoc.Answer(t, ans, 38063)
}
