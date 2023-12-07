package solve

import (
	"strings"
	"testing"

	_ "embed"

	aoc "gitlab.com/vikblom/advent-of-code"
)

var (
	//go:embed "input.txt"
	input      string
	inputLines = strings.Split(strings.TrimRight(input, "\n"), "\n")
)

func TestPartOne(t *testing.T) {
	sum := 0
	for _, s := range inputLines {
		_, s, _ := strings.Cut(s, ":")
		left, right, _ := strings.Cut(s, "|")

		winner := map[string]bool{}
		for _, w := range strings.Split(left, " ") {
			if len(w) > 0 {
				winner[w] = true
			}
		}

		wins := 0
		for _, n := range strings.Split(right, " ") {
			if _, ok := winner[n]; ok {
				wins += 1
			}
		}
		if wins > 0 {
			sum += (1 << (wins - 1)) // 2 ^ (#wins - 1)
		}
	}

	aoc.Answer(t, sum, 23847)
}

func TestPartTwo(t *testing.T) {
	cards := make([]int, len(inputLines))
	for i := 0; i < len(cards); i++ {
		cards[i] = 1
	}

	for i, s := range inputLines {
		_, s, _ := strings.Cut(s, ":")
		left, right, _ := strings.Cut(s, "|")

		winner := map[string]bool{}
		for _, w := range strings.Split(left, " ") {
			if len(w) > 0 {
				winner[w] = true
			}
		}

		wins := 0
		for _, n := range strings.Split(right, " ") {
			if _, ok := winner[n]; ok {
				wins += 1
				cards[i+wins] += cards[i]
			}
		}
	}

	sum := 0
	for _, c := range cards {
		sum += c
	}
	aoc.Answer(t, sum, 8570000)
}
