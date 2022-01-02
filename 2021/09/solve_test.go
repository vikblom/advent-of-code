package solve

import (
	"bytes"
	"fmt"
	"log"
	"os"
	"sort"
	"strings"
	"testing"
)

type Pt struct {
	row int
	col int
}

type Map struct {
	rows int
	cols int
	data []int // row major
}

func (m Map) String() string {
	var s strings.Builder
	for i, v := range m.data {
		if i > 0 && i%m.cols == 0 {
			fmt.Fprintf(&s, "\n")
		}
		fmt.Fprintf(&s, "%d", v)
	}
	return s.String()
}

func (m Map) NeighbourValues(row, col int) []int {
	nbrs := []int{}
	if row > 0 {
		nbrs = append(nbrs, m.At(row-1, col))
	}
	if row < (m.rows - 1) {
		nbrs = append(nbrs, m.At(row+1, col))
	}

	if col > 0 {
		nbrs = append(nbrs, m.At(row, col-1))
	}
	if col < (m.cols - 1) {
		nbrs = append(nbrs, m.At(row, col+1))
	}
	return nbrs
}

func (m Map) At(row, col int) int {
	index := row*m.cols + col
	return m.data[index]
}

func (m Map) Set(row, col, val int) {
	index := row*m.cols + col
	m.data[index] = val
}

func Part1(karta Map) (int, error) {
	sum := 0
	for row := 0; row < karta.rows; row++ {
		for col := 0; col < karta.cols; col++ {
			v := karta.At(row, col)
			nbrs := karta.NeighbourValues(row, col)

			min := true
			for _, n := range nbrs {
				if n <= v {
					min = false
					break
				}
			}
			if min {
				sum += v + 1
			}
		}
	}
	return sum, nil
}

func (m Map) Neighbours(row, col int) []Pt {
	nbrs := []Pt{}
	if row > 0 {
		nbrs = append(nbrs, Pt{row - 1, col})
	}
	if row < (m.rows - 1) {
		nbrs = append(nbrs, Pt{row + 1, col})
	}

	if col > 0 {
		nbrs = append(nbrs, Pt{row, col - 1})
	}
	if col < (m.cols - 1) {
		nbrs = append(nbrs, Pt{row, col + 1})
	}
	return nbrs
}

func (m Map) FillBasin(row, col int) int {
	next := []Pt{{row, col}}
	size := 0
	for len(next) > 0 {
		p := next[0]
		next = next[1:]

		if m.At(p.row, p.col) != 9 {
			m.Set(p.row, p.col, 9)
			size++
			next = append(next, m.Neighbours(p.row, p.col)...)
		}

	}
	return size
}

func Part2(m Map) (int, error) {
	size := []int{}

	for row := 0; row < m.rows; row++ {
		for col := 0; col < m.cols; col++ {
			if m.At(row, col) != 9 {
				size = append(size, m.FillBasin(row, col))
			}
		}
	}
	sort.Ints(size)
	return size[len(size)-1] * size[len(size)-2] * size[len(size)-3], nil
}

func TestDay(t *testing.T) {
	input, err := os.ReadFile("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	karta := Map{
		data: []int{},
		rows: 0,
		cols: bytes.IndexByte(input, '\n'),
	}

	for _, c := range input {
		if c == '\n' {
			karta.rows++
		} else {
			karta.data = append(karta.data, int(c-'0'))
		}
	}
	// log.Printf("\n%s", karta)
	// log.Println(karta.NeighbourValues(4, 9))

	ans, err := Part1(karta)
	if err != nil {
		log.Fatal(err)
	}
	log.Println("Part 1:", ans)

	ans, err = Part2(karta)
	if err != nil {
		log.Fatal(err)
	}
	log.Println("Part 2:", ans)

}
