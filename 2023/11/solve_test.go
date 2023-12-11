package solve

import (
	"bytes"
	"fmt"
	"maps"
	"testing"

	aoc "gitlab.com/vikblom/advent-of-code"

	_ "embed"
)

var (
	//go:embed "input.txt"
	raw      []byte
	rawLines = bytes.Split(bytes.TrimRight(raw, "\n"), []byte{'\n'})
)

type XY aoc.XY

func bounding(stars map[XY]bool) (int, int) {
	row, col := 0, 0
	for s := range stars {
		row = max(row, s.X)
		col = max(col, s.Y)
	}
	return row + 1, col + 1
}

func expand(stars map[XY]bool, factor int) {
	rows, cols := bounding(stars)
	delta := factor - 1

	rowsWithStars := make([]bool, rows)
	colsWithStars := make([]bool, rows)
	for s := range stars {
		rowsWithStars[s.X] = true
		colsWithStars[s.Y] = true
	}

	for r := rows - 1; r >= 0; r-- {
		if rowsWithStars[r] {
			continue
		}
		for k, v := range maps.Clone(stars) {
			if k.X > r {
				delete(stars, k)
				stars[XY{k.X + delta, k.Y}] = v
			}
		}
	}

	for c := cols - 1; c >= 0; c-- {
		if colsWithStars[c] {
			continue
		}
		for k, v := range maps.Clone(stars) {
			if k.Y > c {
				delete(stars, k)
				stars[XY{k.X, k.Y + delta}] = v
			}
		}
	}
}

func TestPartOne(t *testing.T) {
	stars := map[XY]bool{}
	for r, l := range rawLines {
		for c, v := range l {
			if v == '#' {
				stars[XY{r, c}] = true
			}
		}
	}
	expand(stars, 2)

	stars2 := []XY{}
	for s := range stars {
		stars2 = append(stars2, s)
	}

	ans := 0
	for i, a := range stars2 {
		if i == len(stars2)-1 {
			break
		}
		for _, b := range stars2[i+1:] {
			ans += abs(b.X-a.X) + abs(b.Y-a.Y)
		}
	}

	aoc.Answer(t, ans, 9723824)
}

func abs(v int) int {
	if v < 0 {
		return -v
	}
	return v
}

func TestPartTwo(t *testing.T) {
	stars := map[XY]bool{}
	for r, l := range rawLines {
		for c, v := range l {
			if v == '#' {
				stars[XY{r, c}] = true
			}
		}
	}

	expand(stars, 1_000_000)

	stars2 := []XY{}
	for s := range stars {
		stars2 = append(stars2, s)
	}

	ans := 0
	for i, a := range stars2 {
		if i == len(stars2)-1 {
			break
		}
		for _, b := range stars2[i+1:] {
			ans += abs(b.X-a.X) + abs(b.Y-a.Y)
		}
	}

	aoc.Answer(t, ans, 731244261352)
}

func dump(stars map[XY]bool) {
	rows, cols := bounding(stars)
	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			if stars[XY{r, c}] {
				fmt.Printf("#")
			} else {
				fmt.Printf(".")
			}
		}
		fmt.Println()
	}
}
