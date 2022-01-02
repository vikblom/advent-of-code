package aoc

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
)

func ScanCSV(data []byte, atEOF bool) (advance int, token []byte, err error) {
	if atEOF && len(data) == 0 {
		return 0, nil, nil
	}
	if i := bytes.IndexByte(data, ','); i >= 0 {
		// We have value with comma after
		return i + 1, data[0:i], nil
	}
	// No more data, no more comma, the remainder is the final value
	if atEOF {
		return len(data), data, nil
	}
	return 0, nil, nil
}

func ReadCSV(filename string) ([]int, error) {
	content, err := os.ReadFile(filename)
	if err != nil {
		return nil, err
	}

	r := bytes.NewBuffer(content)
	sc := bufio.NewScanner(r)
	sc.Split(ScanCSV)

	ints := []int{}
	for sc.Scan() {
		f, err := strconv.Atoi(strings.TrimSpace(sc.Text()))
		if err != nil {
			return nil, err
		}
		ints = append(ints, f)
	}
	return ints, nil
}

func ReadInts(rdr io.Reader) ([]int, error) {
	sc := bufio.NewScanner(rdr)
	data := []int{}
	for sc.Scan() {
		x, err := strconv.Atoi(sc.Text())
		if err != nil {
			return nil, err
		}
		data = append(data, x)
	}
	return data, nil
}

type Matrix struct {
	rows int
	cols int
	data []int // row major
}

func NewMatrix(rows, cols int) Matrix {
	return Matrix{
		rows: rows,
		cols: cols,
		data: make([]int, rows*cols),
	}
}

func IntSqrt(n int) int {
	x := n
	y := 1
	for x > y {
		x = (x + y) / 2
		y = n / x
	}
	return x
}

func ToMatrix(data []int, rows, cols int) Matrix {
	return Matrix{
		data: data,
		rows: rows,
		cols: cols,
	}
}

func (m Matrix) Width() int {
	return m.cols
}

func (m Matrix) Height() int {
	return m.rows
}

func (m Matrix) String() string {
	var s strings.Builder
	for i, v := range m.data {
		if i > 0 && i%m.cols == 0 {
			fmt.Fprintf(&s, "\n")
		}
		if v > 0 {
			fmt.Fprintf(&s, "%v", v)
		} else {
			fmt.Fprintf(&s, ".")
		}
	}
	return s.String()
}

type XY struct {
	X int
	Y int
}

func (m Matrix) Neighbours(row, col int) []XY {
	nbrs := []XY{}
	if row > 0 {
		nbrs = append(nbrs, XY{row - 1, col})
	}
	if row < (m.rows - 1) {
		nbrs = append(nbrs, XY{row + 1, col})
	}

	if col > 0 {
		nbrs = append(nbrs, XY{row, col - 1})
	}
	if col < (m.cols - 1) {
		nbrs = append(nbrs, XY{row, col + 1})
	}
	return nbrs
}

func (m Matrix) At(row, col int) int {
	index := row*m.cols + col
	return m.data[index]
}

func (m Matrix) Set(row, col, val int) {
	index := row*m.cols + col
	m.data[index] = val
}

func (m Matrix) Increment(row, col int) {
	index := row*m.cols + col
	m.data[index]++
}

func MaxInt(a, b int) int {
	if a > b {
		return a
	} else {
		return b
	}
}

func MinInt(a, b int) int {
	if a < b {
		return a
	} else {
		return b
	}
}

func AbsInt(a int) int {
	if a < 0 {
		return -a
	}
	return a
}
