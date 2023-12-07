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

func parseInts(s string) []int {
	out := []int{}
	_, s, _ = strings.Cut(s, ":")
	for _, s := range strings.Fields(s) {
		out = append(out, aoc.MustInt(s))
	}
	return out
}

func TestPartOne(t *testing.T) {
	time := parseInts(inputLines[0])
	dist := parseInts(inputLines[1])

	ans := 1
	for i := 0; i < len(time); i++ {
		wins := 0
		// Charge longer and longer
		for x := 1; x < time[i]; x++ {
			d := (time[i] - x) * x
			if d > dist[i] {
				wins += 1
			}
		}
		ans *= wins
	}

	aoc.Answer(ans, 138915)
}

func TestPartTwo(t *testing.T) {
	time := parseInts(strings.ReplaceAll(inputLines[0], " ", ""))
	dist := parseInts(strings.ReplaceAll(inputLines[1], " ", ""))

	ans := 1
	for i := 0; i < len(time); i++ {
		wins := 0
		// Charge longer and longer
		for x := 1; x < time[i]; x++ {
			d := (time[i] - x) * x
			if d > dist[i] {
				wins += 1
			}
		}
		ans *= wins
	}

	aoc.Answer(ans, 27340847)
}
