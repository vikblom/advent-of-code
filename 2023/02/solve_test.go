package solve

import (
	"bytes"
	"strings"
	"testing"

	_ "embed"

	aoc "gitlab.com/vikblom/advent-of-code"
)

var (

	//go:embed "input.txt"
	raw      []byte
	rawLines = bytes.Split(bytes.TrimRight(raw, "\n"), []byte{'\n'})

	//go:embed "input.txt"
	input      string
	inputLines = strings.Split(strings.TrimRight(input, "\n"), "\n")
)

var limits = map[string]int{
	"red":   12,
	"green": 13,
	"blue":  14,
}

func TestPartOne(t *testing.T) {
	var sum = 0
nextgame:
	for _, l := range inputLines {
		v, rest, _ := strings.Cut(l, ": ")
		_, vv, _ := strings.Cut(v, "Game ")
		id := aoc.MustInt(vv)

		for _, round := range strings.Split(rest, ";") {
			round = strings.TrimSpace(round)
			for _, reveal := range strings.Split(round, ",") {
				reveal = strings.TrimSpace(reveal)
				n, color, ok := strings.Cut(reveal, " ")
				nn := aoc.MustInt(n)
				limit, ok := limits[color]
				if !ok {
					panic("bad color")
				}
				if nn > limit {
					continue nextgame
				}
			}

		}
		sum += id
	}

	aoc.Answer(sum, 2283)
}

func TestPartTwo(t *testing.T) {
	sum := 0
	for _, l := range inputLines {
		_, rest, _ := strings.Cut(l, ": ")

		dice := map[string]int{}

		for _, round := range strings.Split(rest, ";") {
			round = strings.TrimSpace(round)
			for _, reveal := range strings.Split(round, ",") {
				reveal = strings.TrimSpace(reveal)
				n, color, ok := strings.Cut(reveal, " ")
				if !ok {
					panic("bad color")
				}
				nn := aoc.MustInt(n)
				dice[color] = max(dice[color], nn)
			}

		}
		pow := 1
		for _, d := range dice {
			pow *= d
		}
		sum += pow
	}

	aoc.Answer(sum, 78669)
}
