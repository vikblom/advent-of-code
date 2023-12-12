package solve

import (
	"encoding/binary"
	"fmt"
	"hash/fnv"
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

var (
	hasher = fnv.New64()
	buf    = make([]byte, 0, 512)
)

// Go cannot hash a slice, and building strings as keys
// is expensive, so we hash it ourselves.
// This will however be hashed again by the map.
func hash(s string, vs []int) uint64 {
	defer hasher.Reset()
	defer func() {
		hasher.Reset()
		buf = buf[:0]
	}()

	buf = append(buf, []byte(s)...)
	for _, v := range vs {
		buf = binary.BigEndian.AppendUint64(buf, uint64(v))
	}
	hasher.Write(buf)
	return hasher.Sum64()
}

var mem = map[uint64]int{}

func combos(springs string, groups []int) (ret int) {
	// Memoized.
	key := hash(springs, groups)
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
		c := combos(springs, groups)
		t.Logf("%v %v %d\n", springs, groups, c)
		ans += c
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

		ans += combos(springs, groups)
	}

	aoc.Answer(t, ans, 1537505634471)
}
