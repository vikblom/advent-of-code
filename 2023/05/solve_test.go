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
	aoc.Answer(t, ans, 579439039)
}

func TestPartTwo(t *testing.T) {
	t.Skip("Replaced")

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
	aoc.Answer(t, ans, 7873084)
}

type Interval struct {
	start, end, x int
}

func TestPartTwoRanges(t *testing.T) {
	_, header, _ := strings.Cut(inputLines[0], ": ")
	raw := strings.Split(header, " ")
	seeds := []Interval{}
	for i := 0; i < len(raw); i += 2 {
		a := aoc.MustInt(raw[i])
		r := aoc.MustInt(raw[i+1])
		seeds = append(seeds, Interval{
			start: a,
			end:   a + r,
		})
	}

	chain := [][]Interval{}
	this := []Interval{}
	for _, block := range strings.Split(strings.TrimRight(input, "\n"), "\n\n")[1:] {
		lines := strings.Split(block, "\n")
		for _, l := range lines[1:] {
			nums := strings.Split(l, " ")
			ints := []int{}
			for _, n := range nums {
				ints = append(ints, aoc.MustInt(n))
			}
			dest, src, length := ints[0], ints[1], ints[2]
			this = append(this, Interval{start: src, end: src + length, x: dest - src})
		}

		slices.SortFunc(this, func(a, b Interval) int { return a.start - b.start })
		chain = append(chain, this)
		this = []Interval{}
	}

	for _, maps := range chain {
		next := []Interval{}
	next:
		for i := 0; i < len(seeds); i++ {
			a := seeds[i].start
			b := seeds[i].end
			for _, m := range maps {
				c := m.start
				d := m.end
				// fmt.Printf("[%d %d)  over [%d %d) + %d\n", a, b, c, d, m.x)
				switch {
				case d <= a:
					// Keep looking, maps will be bigger and bigger.
					//      a b
					// c d
					continue
				case b <= c:
					// Went past, keep the same.
					// a b              => [a b)
					//     c d
					next = append(next, Interval{
						start: a,
						end:   b,
					})
					continue next
				case c <= a && b <= d:
					//   a b            => [a b)+x
					// c     d
					next = append(next, Interval{
						start: a + m.x,
						end:   b + m.x,
					})
					continue next
				case c <= b && b < d:
					// a   b           => [a c) [c, b)+x
					//   c   d
					next = append(next, Interval{
						start: c + m.x,
						end:   b + m.x,
					})
					// [a c) does not carry since it has already been passed.
					continue next
				case a < d && d < b:
					//   a   b          => [a d)+x [d b)
					// c   d
					next = append(next, Interval{
						start: a + m.x,
						end:   d + m.x,
					})
					if d < b {
						seeds = append(seeds, Interval{
							start: d,
							end:   b,
						})
					}
					continue next

				default:
					panic("unreachable")
				}
			}
			next = append(next, Interval{
				start: a,
				end:   b,
			})
		}
		seeds = next[:]
	}

	slices.SortFunc(seeds, func(a, b Interval) int { return a.start - b.start })
	aoc.Answer(t, seeds[0].start, 7873084)
}
