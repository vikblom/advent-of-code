package solve

import (
	"slices"
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

func delta(xs []int) []int {
	if len(xs) < 2 {
		panic("bad input")
	}
	prev := xs[0]
	deltas := []int{}
	for _, at := range xs[1:] {
		deltas = append(deltas, at-prev)
		prev = at
	}
	return deltas
}

func allZero(xs []int) bool {
	for _, x := range xs {
		if x != 0 {
			return false
		}
	}
	return true
}

// Recursive approach like /u/gemdude46.
func extrapolate(xs []int) int {
	if allZero(xs) {
		return 0
	}
	return xs[len(xs)-1] + extrapolate(delta(xs))
}

func TestPartOne(t *testing.T) {
	ans := 0
	for _, line := range inputLines {
		is := []int{}
		for _, f := range strings.Fields(line) {
			is = append(is, aoc.MustInt(f))
		}
		ans += extrapolate(is)
	}
	aoc.Answer(t, ans, 1987402313)
}

func TestPartTwo(t *testing.T) {
	ans := 0
	for _, line := range inputLines {
		strings.Fields(line)
		is := []int{}
		for _, f := range strings.Fields(line) {
			is = append(is, aoc.MustInt(f))
		}
		slices.Reverse(is)
		ans += extrapolate(is)
	}
	aoc.Answer(t, ans, 900)
}
