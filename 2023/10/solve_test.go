package solve

import (
	"bytes"
	"fmt"
	"maps"
	"slices"
	"testing"

	aoc "gitlab.com/vikblom/advent-of-code"

	_ "embed"
)

var (
	//go:embed "input.txt"
	raw      []byte
	rawLines = bytes.Split(bytes.TrimRight(raw, "\n"), []byte{'\n'})
)

type XY struct {
	x, y int
}

func readPipes() (XY, map[XY]byte) {
	var start XY
	pipes := map[XY]byte{}
	for x, l := range rawLines {
		for y, c := range l {
			switch c {
			case 'S':
				start = XY{x, y}
			case '.':
				// skip.
			default:
				pipes[XY{x, y}] = c
			}
		}
	}
	pipes[start] = replaceStart(pipes, start)

	return start, pipes
}

// replaceStart with a character that connects to its neighbours.
func replaceStart(pipes map[XY]byte, at XY) byte {
	nbrs := func(at XY, bs ...byte) bool {
		return slices.Contains(bs, pipes[at])
	}

	above := nbrs(XY{at.x - 1, at.y}, 'F', '7', '|')
	below := nbrs(XY{at.x + 1, at.y}, 'J', 'L', '|')
	left := nbrs(XY{at.x, at.y - 1}, 'F', 'L', '-')
	right := nbrs(XY{at.x, at.y + 1}, '7', 'J', '-')

	switch {
	case below && right:
		return 'F'
	case left && above:
		return 'J'
	case left && below:
		return '7'
	case above && right:
		return 'L'
	case left && right:
		return '-'
	case above && below:
		return '|'
	default:
		panic("impossible start")
	}
}

// nextPipe in the loop.
func nextPipe(pipes map[XY]byte, at, prev XY) XY {
	switch pipes[at] {
	case '|':
		if at.x > prev.x {
			return XY{at.x + 1, at.y}
		}
		return XY{at.x - 1, at.y}
	case '-':
		if at.y > prev.y {
			return XY{at.x, at.y + 1}
		}
		return XY{at.x, at.y - 1}
	case 'F':
		if at.x < prev.x {
			return XY{at.x, at.y + 1}
		}
		return XY{at.x + 1, at.y}
	case 'J':
		if at.x > prev.x {
			return XY{at.x, at.y - 1}
		}
		return XY{at.x - 1, at.y}
	case '7':
		if at.x < prev.x {
			return XY{at.x, at.y - 1}
		}
		return XY{at.x + 1, at.y}
	case 'L':
		if at.x > prev.x {
			return XY{at.x, at.y + 1}
		}
		return XY{at.x - 1, at.y}
	}

	panic("unreachable")
}

func TestPartOne(t *testing.T) {
	start, pipes := readPipes()

	at := start
	prev := start
	len := 0
	for {
		at, prev = nextPipe(pipes, at, prev), at
		len += 1
		if at == start {
			break
		}
	}

	aoc.Answer(t, len/2, 7102)
}

func TestPartTwo(t *testing.T) {
	start, pipes := readPipes()

	loop := []XY{start}
	at := start
	// Pick a direction at random, but count the turns
	// and correct for it after we have the full pipe.
	prev := start
	turns := 0
	for {
		next := nextPipe(pipes, at, prev)

		// 2d cross product: (at-prev) x (next-prev).
		a := XY{at.x - prev.x, at.y - prev.y}
		b := XY{next.x - prev.x, next.y - prev.y}
		turns += a.x*b.y - a.y*b.x

		at, prev = next, at
		if at == start {
			break
		}
		loop = append(loop, at)
	}
	// We must have a right hand turn for the hard-coded fill
	// on the right hand side to work as expected.
	if turns > 0 {
		slices.Reverse(loop)
	}

	blocked := map[XY]bool{}
	filled := map[XY]bool{}
	prev = loop[len(loop)-1]
	for _, at := range loop {
		// Mark the pipeline itself.
		blocked[at] = true
		// Fill the right hand side.
		// Overfill and remove overlap with loop later.
		switch {
		case at.y == prev.y:
			if prev.x < at.x {
				filled[XY{at.x, at.y - 1}] = true
				filled[XY{prev.x, prev.y - 1}] = true
			} else {
				filled[XY{at.x, at.y + 1}] = true
				filled[XY{prev.x, prev.y + 1}] = true
			}
		case at.x == prev.x:
			if prev.y < at.y {
				filled[XY{at.x + 1, at.y}] = true
				filled[XY{prev.x + 1, prev.y}] = true
			} else {
				filled[XY{at.x - 1, at.y}] = true
				filled[XY{prev.x - 1, prev.y}] = true
			}
		}

		prev = at
	}
	maps.DeleteFunc(filled, func(xy XY, _ bool) bool {
		return blocked[xy]
	})

	for {
		fill := map[XY]bool{}
		for at := range filled {
			for _, n := range nbrs4(at) {
				if !filled[n] && !blocked[n] {
					fill[n] = true
				}
			}
		}

		maps.Copy(filled, fill)
		if len(fill) == 0 {
			break
		}
	}

	aoc.Answer(t, len(filled), 363)
}

func nbrs4(at XY) []XY {
	return []XY{
		{at.x + 1, at.y},
		{at.x - 1, at.y},
		{at.x, at.y + 1},
		{at.x, at.y - 1},
	}
}

func dump(loop, filled map[XY]bool) {
	for x, l := range rawLines {
		for y, c := range l {
			at := XY{x, y}
			switch {
			case loop[at]:
				fmt.Printf("%s", string(c))
			case filled[at]:
				fmt.Printf("·")
			default:
				fmt.Printf(" ")
			}
		}
		fmt.Printf("\n")
	}
}
