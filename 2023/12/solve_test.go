package solve

import (
	"fmt"
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

func fits(s string, size int) (ret bool) {
	if len(s) < size {
		return false
	}
	if strings.Contains(s[:size], ".") {
		return false
	}
	return (size == len(s)) || s[size] != '#'
}

var mem = map[int]int{}

func combos(springs string, groups []int) (ret int) {
	// Memoized each individual row.
	// The loss of caching between rows is insignificant.
	// And then the length of strings and groups uniquely decides state.
	key := len(springs)<<32 + len(groups)
	if ans, ok := mem[key]; ok {
		return ans
	}
	defer func() {
		mem[key] = ret
	}()

	if len(groups) == 0 {
		if strings.Contains(springs, "#") {
			return 0
		}
		return 1
	}
	agg := 0
	for i := 0; i < len(springs); i++ {
		if fits(springs[i:], groups[0]) {
			// Here we cut off the next
			next := i + groups[0]
			if next < len(springs) {
				next++
			}
			agg += combos(springs[next:], groups[1:])
		}
		// Cannot skip a #, since then the groups would not
		// account for all contigious blocks!
		if springs[i] == '#' {
			break
		}
	}
	return agg
}

func TestPartOne(t *testing.T) {
	ans := 0
	for _, l := range inputLines {
		springs, rest, _ := strings.Cut(l, " ")
		groups := []int{}
		for _, c := range strings.Split(rest, ",") {
			groups = append(groups, aoc.MustInt(c))
		}
		mem = map[int]int{}
		ans += combos(springs, groups)
	}
	aoc.Answer(t, ans, 6827)
}

func TestPartTwo(t *testing.T) {
	ans := 0
	for _, l := range inputLines {
		springs, rest, _ := strings.Cut(l, " ")
		groups := []int{}
		rest = fmt.Sprintf("%s,%s,%s,%s,%s", rest, rest, rest, rest, rest)
		for _, c := range strings.Split(rest, ",") {
			groups = append(groups, aoc.MustInt(c))
		}

		springs = fmt.Sprintf("%s?%s?%s?%s?%s", springs, springs, springs, springs, springs)

		mem = map[int]int{}
		ans += combos(springs, groups)
	}

	aoc.Answer(t, ans, 1537505634471)
}
