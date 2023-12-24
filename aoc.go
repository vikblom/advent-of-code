package aoc

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os"
	"reflect"
	"strconv"
	"strings"
	"testing"
)

func Answer(t *testing.T, got any, want ...any) {
	t.Helper()

	if len(want) == 0 {
		t.Errorf("ANSWER: got %v\n", got)
		return
	}

	if !reflect.DeepEqual(got, want[0]) {
		t.Errorf("INCORRECT: got %v != want %v\n", got, want[0])
		return
	}
	t.Logf("CORRECT: got %v == want %v\n", got, want[0])
}

func MustInt(s string) int {
	i, err := strconv.Atoi(s)
	if err != nil {
		panic(err)
	}
	return i
}

func MustFloat(s string) float64 {
	i, err := strconv.ParseFloat(s, 64)
	if err != nil {
		panic(err)
	}
	return i
}

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

type Matrix[T any] struct {
	Rows int
	Cols int
	data []T // row major
}

func NewMatrix[T any](rows, cols int) Matrix[T] {
	return Matrix[T]{
		Rows: rows,
		Cols: cols,
		data: make([]T, rows*cols),
	}
}

func ParseMatrix(s string) Matrix[byte] {
	cols := strings.Index(s, "\n")
	data := strings.ReplaceAll(s, "\n", "")
	return ToMatrix([]byte(data), len(data)/cols, cols)
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

func ToMatrix[T any](data []T, rows, cols int) Matrix[T] {
	return Matrix[T]{
		data: data,
		Rows: rows,
		Cols: cols,
	}
}

func (m Matrix[T]) At(row, col int) T {
	index := row*m.Cols + col
	return m.data[index]
}

func (m Matrix[T]) Slice() []T {
	return m.data
}

func (m Matrix[T]) Set(row, col int, val T) {
	index := row*m.Cols + col
	m.data[index] = val
}

func (m Matrix[T]) Width() int {
	return m.Cols
}

func (m Matrix[T]) Height() int {
	return m.Rows
}

func (m Matrix[T]) Inbounds(x, y int) bool {
	return 0 <= x && x < m.Rows && 0 <= y && y < m.Cols
}

func (m Matrix[T]) String() string {
	var s strings.Builder
	for i, v := range m.data {
		if i > 0 && i%m.Cols == 0 {
			fmt.Fprintf(&s, "\n")
		}
		// Trick the switch into accepting a generic.
		switch vv := any(v).(type) {
		case byte:
			fmt.Fprintf(&s, "%c", vv)
		default:
			fmt.Fprintf(&s, "%v", vv)
		}
	}
	return s.String()
}

func zero[T any]() T {
	z := new(T)
	return *z
}

type XY struct {
	X int
	Y int
}

func (m Matrix[T]) Neighbours(row, col int) []XY {
	nbrs := []XY{}
	if row > 0 {
		nbrs = append(nbrs, XY{row - 1, col})
	}
	if row < (m.Rows - 1) {
		nbrs = append(nbrs, XY{row + 1, col})
	}

	if col > 0 {
		nbrs = append(nbrs, XY{row, col - 1})
	}
	if col < (m.Cols - 1) {
		nbrs = append(nbrs, XY{row, col + 1})
	}
	return nbrs
}

func (m Matrix[T]) Nbrs8(row, col int) []XY {
	nbrs := []XY{}
	for i := max(row-1, 0); i < min(row+2, m.Height()); i++ {
		for j := max(col-1, 0); j < min(col+2, m.Width()); j++ {
			nbrs = append(nbrs, XY{X: i, Y: j})
		}
	}
	return nbrs
}

// func (m Matrix) Increment(row, col int) {
// 	index := row*m.cols + col
// 	m.data[index]++
// }

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

func Scanner(bs []byte) *bufio.Scanner {
	return bufio.NewScanner(bytes.NewReader(bs))
}

// Greatest common divisor.
func GCD(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

// Least common multiplier.
func LCM(a int, bs ...int) int {
	for _, b := range bs {
		a = a * b / GCD(a, b)
	}
	return a
}
