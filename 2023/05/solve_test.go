package solve

import (
	"slices"
	"strings"
	"sync"
	"testing"

	_ "embed"

	aoc "gitlab.com/vikblom/advent-of-code"
)

var (
	//go:embed "input.txt"
	input      string
	inputLines = strings.Split(strings.TrimRight(input, "\n"), "\n")
)

type Range struct {
	src, dest, len int
}

// Chain one seed to its location.
// Apply one range per item in chain, or keep the value if there is no match.
func Chain(chain [][]Range, s int) int {
	for _, c := range chain {
		for _, cc := range c {
			d := s - cc.src
			if 0 <= d && d < cc.len {
				s = cc.dest + d
				break
			}
		}
	}
	return s
}

func TestPartOne(t *testing.T) {
	_, header, _ := strings.Cut(inputLines[0], ": ")
	seeds := []int{}
	for _, s := range strings.Split(header, " ") {
		seeds = append(seeds, aoc.MustInt(s))
	}

	chain := [][]Range{}
	this := []Range{}
	for _, block := range strings.Split(strings.TrimRight(input, "\n"), "\n\n")[1:] {
		lines := strings.Split(block, "\n")
		for _, l := range lines[1:] {
			nums := strings.Split(l, " ")
			ints := []int{}
			for _, n := range nums {
				ints = append(ints, aoc.MustInt(n))
			}
			dest, src, length := ints[0], ints[1], ints[2]
			this = append(this, Range{src: src, dest: dest, len: length})
		}

		chain = append(chain, this)
		this = []Range{}
	}

	ans := 1 << 62
	for _, s := range seeds {
		ans = min(ans, Chain(chain, s))
	}
	aoc.Answer(ans, 579439039)
}

func TestPartTwo(t *testing.T) {
	_, header, _ := strings.Cut(inputLines[0], ": ")
	seeds := []int{}
	for _, s := range strings.Split(header, " ") {
		seeds = append(seeds, aoc.MustInt(s))
	}

	chain := [][]Range{}
	this := []Range{}
	for _, block := range strings.Split(strings.TrimRight(input, "\n"), "\n\n")[1:] {
		lines := strings.Split(block, "\n")
		for _, l := range lines[1:] {
			nums := strings.Split(l, " ")
			ints := []int{}
			for _, n := range nums {
				ints = append(ints, aoc.MustInt(n))
			}
			dest, src, length := ints[0], ints[1], ints[2]
			this = append(this, Range{src: src, dest: dest, len: length})
		}

		slices.SortFunc(this, func(a, b Range) int { return a.src - b.src })
		chain = append(chain, this)
		this = []Range{}
	}

	brute := func(chain [][]Range, start, len int) int {
		ans := 1 << 62
		for i := 0; i < len; i++ {
			ans = min(ans, Chain(chain, start+i))
		}
		return ans
	}

	var wg sync.WaitGroup
	out := make([]int, len(seeds)/2)
	for i := 0; i < len(seeds); i += 2 {
		i := i
		wg.Add(1)
		go func() {
			out[i/2] = brute(chain, seeds[i], seeds[i+1])
			wg.Done()
		}()
	}
	wg.Wait()

	ans := out[0]
	for _, v := range out {
		ans = min(ans, v)
	}
	aoc.Answer(ans, 7873084)
}
