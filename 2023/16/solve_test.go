package solve

import (
	"strings"
	"sync"
	"testing"

	aoc "gitlab.com/vikblom/advent-of-code"

	_ "embed"
)

var (
	//go:embed "input.txt"
	input      string
	inputLines = strings.Split(strings.TrimRight(input, "\n"), "\n")
)

type XY aoc.XY

type Heading int

const (
	North Heading = 1 << iota
	East
	South
	West
)

type Beam struct {
	x, y int
	h    Heading
}

func Bounce(b Beam, on byte) []Heading {
	switch on {
	case '.':
		return []Heading{b.h}

	case '/':
		switch b.h {
		case North:
			return []Heading{East}
		case East:
			return []Heading{North}
		case West:
			return []Heading{South}
		case South:
			return []Heading{West}
		}

	case '\\':
		switch b.h {
		case North:
			return []Heading{West}
		case East:
			return []Heading{South}
		case West:
			return []Heading{North}
		case South:
			return []Heading{East}
		}

	case '-':
		switch b.h {
		case North, South:
			return []Heading{West, East}
		case East:
			return []Heading{East}
		case West:
			return []Heading{West}
		}

	case '|':
		switch b.h {
		case North:
			return []Heading{North}
		case East, West:
			return []Heading{North, South}
		case South:
			return []Heading{South}
		}
	}
	panic("bad input")
}

func Energize(m aoc.Matrix[byte], start Beam) int {
	// Bitmask value of which headings the coordinate has been passed with.
	// Cuts off loops and contains our answer.
	energized := map[XY]int{}

	var b Beam
	beams := []Beam{start}
	for len(beams) > 0 {
		// Entering b coordinate.
		b, beams = beams[0], beams[1:]

		// Terminate at out of bounds or loops.
		if !(0 <= b.x && b.x < m.Rows && 0 <= b.y && b.y < m.Cols) {
			continue
		}
		if energized[XY{b.x, b.y}]&int(b.h) > 0 {
			continue
		}
		energized[XY{b.x, b.y}] |= int(b.h)

		for _, h := range Bounce(b, m.At(b.x, b.y)) {
			switch h {
			case North:
				beams = append(beams, Beam{b.x - 1, b.y, North})
			case East:
				beams = append(beams, Beam{b.x, b.y + 1, East})
			case West:
				beams = append(beams, Beam{b.x, b.y - 1, West})
			case South:
				beams = append(beams, Beam{b.x + 1, b.y, South})
			}
		}
	}

	return len(energized)
}

func TestPartOne(t *testing.T) {
	m := aoc.ParseMatrix(input)
	ans := Energize(m, Beam{0, 0, East})
	aoc.Answer(t, ans, 7392)
}

func TestPartTwo(t *testing.T) {
	m := aoc.ParseMatrix(input)

	energies := make(chan int, 64)

	var wg sync.WaitGroup
	solve := func(b Beam) {
		wg.Add(1)
		go func() {
			energies <- Energize(m, b)
			wg.Done()
		}()
	}
	for r := 0; r < m.Rows; r++ {
		solve(Beam{x: r, y: 0, h: East})
		solve(Beam{x: m.Cols - 1, y: 0, h: West})
	}
	for c := 0; c < m.Cols; c++ {
		solve(Beam{x: 0, y: c, h: South})
		solve(Beam{x: m.Rows - 1, y: c, h: North})
	}
	go func() {
		wg.Wait()
		close(energies)
	}()

	ans := 0
	for e := range energies {
		ans = max(ans, e)
	}
	aoc.Answer(t, ans, 7665)
}
