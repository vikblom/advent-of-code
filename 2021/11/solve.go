package main

import (
	"fmt"
	"log"
	"os"
	"strings"
)

type Octopi struct {
	// Row major
	data []int
	rows int
	cols int
}

func (o *Octopi) String() string {
	var s strings.Builder
	for i, v := range o.data {
		if i > 0 && i%o.cols == 0 {
			fmt.Fprintf(&s, "\n")
		}
		if v > 0 {
			fmt.Fprintf(&s, "%d", v)
		} else {
			fmt.Fprintf(&s, ".")
		}
	}
	return s.String()
}

func (o *Octopi) LinearToRowCol(linear int) (int, int) {
	col := linear % o.cols
	row := linear / o.cols
	return row, col
}

func (o *Octopi) RowColToLinear(row, col int) int {
	return row*o.cols + col
}

func (o *Octopi) Neighbours(linear int) []int {

	row, col := o.LinearToRowCol(linear)

	minRow := row - 1
	if minRow < 0 {
		minRow = row
	}
	maxRow := row + 1
	if maxRow >= o.rows {
		maxRow = row
	}
	minCol := col - 1
	if minCol < 0 {
		minCol = 0
	}
	maxCol := col + 1
	if maxCol >= o.cols {
		maxCol = col
	}

	nbrs := []int{}
	for r := minRow; r <= maxRow; r++ {
		for c := minCol; c <= maxCol; c++ {
			if !(r == row && c == col) {
				nbrs = append(nbrs, o.RowColToLinear(r, c))
			}
		}
	}

	return nbrs
}

func (o *Octopi) flash(i int, flashed []bool) {
	flashed[i] = true
	for _, j := range o.Neighbours(i) {
		o.data[j]++
		if o.data[j] > 9 && !flashed[j] {
			o.flash(j, flashed)
		}
	}
}

func (o *Octopi) Step() int {
	flashed := make([]bool, len(o.data))
	for i := 0; i < len(o.data); i++ {
		o.data[i]++
	}
	for i := 0; i < len(o.data); i++ {
		if o.data[i] > 9 && !flashed[i] {
			o.flash(i, flashed)
		}
	}
	sum := 0
	for i, v := range flashed {
		if v {
			o.data[i] = 0
			sum++
		}
	}
	return sum
}

func Part1(input []byte) (int, error) {
	data := []int{}
	for _, r := range input {
		if r == '\n' {
			continue
		}
		data = append(data, int(r-'0'))
	}
	o := Octopi{
		data: data,
		rows: 10,
		cols: 10,
	}

	flashes := 0
	for i := 0; i < 100; i++ {
		flashes += o.Step()
	}
	return flashes, nil
}

func Part2(input []byte) (int, error) {
	data := []int{}
	for _, r := range input {
		if r == '\n' {
			continue
		}
		data = append(data, int(r-'0'))
	}
	o := Octopi{
		data: data,
		rows: 10,
		cols: 10,
	}

	step := 0
	for {
		step++
		flashes := o.Step()
		if flashes == len(o.data) {
			break
		}
	}
	return step, nil
}

func main() {
	input, err := os.ReadFile("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	ans, err := Part1(input)
	if err != nil {
		log.Fatal(err)
	}
	log.Println("Part 1:", ans)

	ans, err = Part2(input)
	if err != nil {
		log.Fatal(err)
	}
	log.Println("Part 2:", ans)

}
