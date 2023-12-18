package solve

import (
	"fmt"
	"strconv"
	"strings"
	"testing"

	aoc "gitlab.com/vikblom/advent-of-code"

	_ "embed"
)

var (
	//go:embed "input.txt"
	input string
	lines = strings.Split(strings.TrimRight(input, "\n"), "\n")
)

type XY aoc.XY

func TestPartOne(t *testing.T) {
	at := XY{X: 0, Y: 0}
	loop := []XY{at}
	perim := 0

	for _, l := range lines {
		ls := strings.Fields(l)
		dir, dist := ls[0], aoc.MustInt(ls[1])

		var delta XY
		switch dir {
		case "U":
			delta = XY{-1, 0}
		case "R":
			delta = XY{0, 1}
		case "D":
			delta = XY{1, 0}
		case "L":
			delta = XY{0, -1}
		default:
			panic("bad input")
		}

		// Jump straight to corners, counting perimeter nodes.
		at = XY{at.X + dist*delta.X, at.Y + dist*delta.Y}
		perim += dist
		loop = append(loop, at)
	}

	// Shoelace theorem.
	shoe := loop[len(loop)-1].X*loop[0].Y - loop[0].X*loop[len(loop)-1].Y
	for i := 0; i < len(loop)-1; i++ {
		shoe += loop[i].X*loop[i+1].Y - loop[i+1].X*loop[i].Y
	}
	shoe = max(shoe, -shoe) / 2

	// Pick's formula relates area to number of boundary and encircled points.
	pick := shoe + 1 - perim/2

	aoc.Answer(t, pick+perim, 52231)
}

func TestPartTwo(t *testing.T) {
	at := XY{X: 0, Y: 0}
	perim := 0

	shoe := 0
	for _, l := range lines {
		_, raw, _ := strings.Cut(l, "#")
		hexa, dir := raw[:len(raw)-2], raw[len(raw)-2]
		dist64, _ := strconv.ParseInt(hexa, 16, 64)
		dist := int(dist64)

		var delta XY
		switch dir {
		case '3':
			delta = XY{-1, 0}
		case '0':
			delta = XY{0, 1}
		case '1':
			delta = XY{1, 0}
		case '2':
			delta = XY{0, -1}
		default:
			panic("bad input")
		}

		// DET ( x_i  x_i+1 )
		//     ( y_i  y_i+1 )
		x1 := at.X + dist*delta.X
		y1 := at.Y + dist*delta.Y
		shoe += at.X*y1 - x1*at.Y

		// Jump straight to corners, counting perimeter nodes.
		at = XY{at.X + dist*delta.X, at.Y + dist*delta.Y}
		perim += dist
	}
	// Shoelace theorem gives us the area of the encircled area.
	shoe = max(shoe, -shoe) / 2

	// Pick's formula relates area and perimiter to encircled points.
	pick := shoe + 1 - perim/2

	aoc.Answer(t, pick+perim, 57196493937398)
}

func dump(dug map[XY]bool) {
	var maxRow, maxCol int
	for d := range dug {
		maxRow = max(maxRow, d.X)
		maxCol = max(maxCol, d.Y)
	}

	for r := 0; r <= maxRow; r++ {
		for c := 0; c <= maxCol; c++ {
			if dug[XY{r, c}] {
				fmt.Printf("#")
			} else {
				fmt.Printf(".")
			}
		}
		fmt.Println()
	}
	fmt.Println()
}
