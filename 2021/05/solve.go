package main

import (
	"bytes"
	"fmt"
	"log"
	"os"
	"strings"
)

// X is right
// Y is down
type Line struct {
	ax, ay, bx, by int
}

func (l Line) String() string {
	return fmt.Sprintf("%d %d -> %d %d\n", l.ax, l.ay, l.bx, l.by)
}

func ReadLines(input []byte) ([]Line, error) {
	rdr := bytes.NewBuffer(input)
	lines := []Line{}
	var err error
	for err == nil {
		l := Line{}
		_, err = fmt.Fscanf(rdr, "%d,%d -> %d,%d\n", &l.ax, &l.ay, &l.bx, &l.by)
		if err == nil {
			lines = append(lines, l)
		}
	}
	return lines, nil
}

func BiggestPoints(lines []Line) (int, int) {
	var width, height int
	for _, l := range lines {
		if l.ax > width {
			width = l.ax
		}
		if l.bx > width {
			width = l.bx
		}
		if l.ay > height {
			height = l.ay
		}
		if l.by > height {
			height = l.by
		}
	}
	return width, height
}

type Matrix struct {
	rows int
	cols int
	// Row major
	data []int
}

func NewMatrix(rows, cols int) Matrix {
	return Matrix{
		rows: rows,
		cols: cols,
		data: make([]int, rows*cols),
	}
}

func (m Matrix) String() string {
	var s strings.Builder
	for i, v := range m.data {
		if i > 0 && i%m.cols == 0 {
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

func (m Matrix) Increment(row, col int) {
	index := row*m.cols + col
	m.data[index]++
}

func Part1(lines []Line) (int, error) {
	rows, cols := BiggestPoints(lines)
	m := NewMatrix(rows+1, cols+1)
	for _, l := range lines {
		if l.ax == l.bx { // vertical
			low, high := l.ay, l.by
			if low > high {
				low, high = high, low
			}
			for row := low; row <= high; row++ {
				m.Increment(row, l.ax)
			}
		} else if l.ay == l.by { // horizontal
			low, high := l.ax, l.bx
			if low > high {
				low, high = high, low
			}
			for col := low; col <= high; col++ {
				m.Increment(l.ay, col)
			}
		}
	}
	//println(m.String())

	sum := 0
	for _, d := range m.data {
		if d > 1 {
			sum += 1
		}
	}

	return sum, nil
}

func SortPair(a, b int) (int, int) {
	if a < b {
		return a, b
	} else {
		return b, a
	}
}

func Sign(i int) int {
	if i > 0 {
		return 1
	} else if i < 0 {
		return -1
	} else {
		return 0
	}
}

func Part2(lines []Line) (int, error) {
	rows, cols := BiggestPoints(lines)
	m := NewMatrix(rows+1, cols+1)
	for _, l := range lines {
		if l.ax == l.bx { // vertical
			low, high := SortPair(l.ay, l.by)
			for row := low; row <= high; row++ {
				m.Increment(row, l.ax)
			}
		} else if l.ay == l.by { // horizontal
			low, high := SortPair(l.ax, l.bx)
			if low > high {
				low, high = high, low
			}
			for col := low; col <= high; col++ {
				m.Increment(l.ay, col)
			}
		} else {
			x := l.ax
			y := l.ay

			dx := Sign(l.bx - l.ax)
			dy := Sign(l.by - l.ay)

			for x != l.bx {
				m.Increment(y, x)
				x += dx
				y += dy
			}
			m.Increment(y, x)
		}
	}
	//println(m.String())

	sum := 0
	for _, d := range m.data {
		if d > 1 {
			sum += 1
		}
	}

	return sum, nil
}

func main() {
	input, err := os.ReadFile("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	lines, err := ReadLines(input)
	if err != nil {
		log.Fatal(err)
	}

	ans, err := Part1(lines)
	if err != nil {
		log.Fatal(err)
	}
	log.Println("Part 1:", ans)

	ans, err = Part2(lines)
	if err != nil {
		log.Fatal(err)
	}
	log.Println("Part 2:", ans)

}
