// -*- compile-command: "go test ./2023/01 -v" -*-
package solve

import (
	"bytes"
	"strconv"
	"strings"
	"testing"

	_ "embed"

	aoc "gitlab.com/vikblom/advent-of-code"
)

var (
	_ = aoc.XY{}

	//go:embed "input.txt"
	raw      []byte
	rawLines = bytes.Split(bytes.TrimRight(raw, "\n"), []byte{'\n'})

	//go:embed "input.txt"
	input      string
	inputLines = strings.Split(strings.TrimRight(input, "\n"), "\n")
)

const digits = "1234567890"

func TestPartOne(t *testing.T) {
	var sum = 0
	sc := aoc.Scanner(raw)
	for sc.Scan() {
		s := sc.Text()
		first := strings.IndexAny(s, digits)
		last := strings.LastIndexAny(s, digits)

		n := string([]byte{s[first], s[last]})
		c, err := strconv.Atoi(n)
		if err != nil {
			t.Fatalf("parse: %s", err)
		}
		sum += c
	}
	aoc.Answer(t, sum, 54968)
}

var digits2 = map[string]string{
	"one":   "1",
	"two":   "2",
	"three": "3",
	"four":  "4",
	"five":  "5",
	"six":   "6",
	"seven": "7",
	"eight": "8",
	"nine":  "9",
}

func TestPartTwo(t *testing.T) {
	var sum = 0
	for _, s := range inputLines {

		first := len(s)
		last := 0
		var firstC, lastC string
		for literal, digit := range digits2 {
			if i := strings.Index(s, literal); i >= 0 {
				if i <= first {
					first = i
					firstC = digit
				}
			}
			if i := strings.Index(s, digit); i >= 0 {
				if i <= first {
					first = i
					firstC = digit
				}
			}

			if i := strings.LastIndex(s, literal); i >= 0 {
				if i >= last {
					last = i
					lastC = digit
				}
			}
			if i := strings.LastIndex(s, digit); i >= 0 {
				if i >= last {
					last = i
					lastC = digit
				}
			}
			// t.Logf("at %s :: %s %s (%d) %s (%d)", literal, s, firstC, first, lastC, last)
		}

		n := string(firstC + lastC)
		c, err := strconv.Atoi(n)
		if err != nil {
			t.Fatalf("parse: %s", err)
		}
		sum += c
	}
	aoc.Answer(t, sum, 54094)
}
