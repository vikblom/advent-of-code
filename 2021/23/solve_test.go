package solve

import (
	"bufio"
	"container/heap"
	"fmt"
	"io"
	"log"
	"os"
	"strings"
	"testing"

	aoc "gitlab.com/vikblom/advent-of-code"
)

const (
	WIDTH  = 13
	HEIGHT = 5
)

var energy = map[rune]int{
	'A': 1,
	'B': 10,
	'C': 100,
	'D': 1000,
}

var homecol = map[rune]int{
	'A': 3,
	'B': 5,
	'C': 7,
	'D': 9,
}

func ReadMap(file string) (Map, error) {
	fh, err := os.Open(file)
	if err != nil {
		log.Fatal(err)
	}
	defer fh.Close()
	buf := bufio.NewReader(fh)

	mat := NewMap(HEIGHT, WIDTH)
	row := 0
	col := 0
	for err == nil {
		b, err := buf.ReadByte()
		if err != nil {
			if err == io.EOF {
				break
			} else {
				return mat, err
			}
		}

		if b == '\n' {
			col = 0
			row++
		} else {
			mat.Set(row, col, int(b))
			col++
		}
	}
	return mat, nil
}

type Map struct {
	rows int
	cols int
	data []int // row major
}

func NewMap(rows, cols int) Map {
	return Map{
		rows: rows,
		cols: cols,
		data: make([]int, rows*cols),
	}
}

func (m Map) String() string {
	var s strings.Builder
	for row := 0; row < m.rows; row++ {
		for col := 0; col < m.cols; col++ {
			c := m.At(row, col)
			if c > 0 {
				fmt.Fprintf(&s, "%c", c)
			}
		}
		if row != m.rows-1 {
			fmt.Fprintf(&s, "\n")
		}
	}
	return s.String()
}

func (m Map) At(row, col int) int {
	index := row*m.cols + col
	return m.data[index]
}

func (m Map) Set(row, col, val int) {
	index := row*m.cols + col
	m.data[index] = val
}

func (m Map) Done() bool {
	return m.At(2, 3) == 'A' && m.At(3, 3) == 'A' &&
		m.At(2, 5) == 'B' && m.At(3, 5) == 'B' &&
		m.At(2, 7) == 'C' && m.At(3, 7) == 'C' &&
		m.At(2, 9) == 'D' && m.At(3, 9) == 'D'
}

func (m Map) Equal(other Map) bool {
	if m.cols != other.cols || m.rows != other.rows {
		return false
	}
	for i := range m.data {
		if m.data[i] != other.data[i] {
			return false
		}
	}
	return true
}

func (m Map) Copy() Map {
	return Map{
		rows: m.rows,
		cols: m.cols,
		data: append([]int{}, m.data...),
	}
}

func (s State) AllMovesFrom(row, col int) []*State {
	c := s.m.At(row, col)
	if row == 1 { // corridor
		home := homecol[rune(c)]
		// There must be place to move in
		if s.m.At(2, home) != '.' {
			return nil
		}
		// Can only move in with friends or empty
		nbr := s.m.At(3, home)
		if !(nbr == c || nbr == '.') {
			return nil
		}
		// Check if the corridor is free, note that no ant can be at
		// the opening, so checking (left, right) is fine.
		left := aoc.MinInt(col, home)
		right := aoc.MaxInt(col, home)
		clear := true
		for i := left + 1; i < right; i++ {
			if s.m.At(1, i) != '.' {
				clear = false
				break
			}
		}
		if !clear {
			return nil
		}
		// Actually move.
		d := 2
		if nbr == '.' {
			d = 3
		}
		e := energy[rune(c)] * (aoc.AbsInt(col-home) + aoc.AbsInt(row-d))
		m := s.m.Copy()
		m.Set(row, col, '.')
		m.Set(d, home, c)

		return []*State{
			{
				energy: s.energy + e,
				m:      m,
			},
		}

	} else {
		// If this is deep in correct col,
		// or whoever is deeper is also correct, dont move.
		if col == homecol[rune(c)] && (row == 3 || s.m.At(3, col) == c) {
			//log.Println("ALREADY GOOD")
		} else if row == 3 && s.m.At(2, col) != '.' {
			// Some other ant must get out first.
			//log.Println("LOCKED IN")
		} else {
			// All possible moves into corridor
			left := col
			for s.m.At(1, left) == '.' {
				left--
			}
			right := col
			for s.m.At(1, right) == '.' {
				right++
			}

			news := []*State{}
			for i := left + 1; i < right; i++ {
				if i == 3 || i == 5 || i == 7 || i == 9 {
					continue // cannot block entrances
				}
				e := energy[rune(c)] * (aoc.AbsInt(col-i) + aoc.AbsInt(row-1))
				m := s.m.Copy()
				m.Set(row, col, '.')
				m.Set(1, i, c)
				new := &State{
					energy: s.energy + e,
					m:      m,
				}
				news = append(news, new)
			}
			return news
		}
	}
	return nil
}

func (s *State) Moves() []*State {
	moves := []*State{}
	for row := 0; row < s.m.rows; row++ {
		for col := 0; col < s.m.cols; col++ {
			c := s.m.At(row, col)
			if 'A' <= c && c <= 'D' {
				moves = append(moves, s.AllMovesFrom(row, col)...)
			}
		}
	}
	return moves
}

type State struct {
	m      Map
	energy int

	// index is needed for heap update
	index int
}

type Queue []*State

func (q Queue) Len() int           { return len(q) }
func (q Queue) Less(i, j int) bool { return q[i].energy < q[j].energy }

func (q Queue) Swap(i, j int) {
	q[i], q[j] = q[j], q[i]
	q[i].index = i
	q[j].index = j
}

func (q *Queue) Push(x interface{}) {
	n := len(*q)
	item := x.(*State)
	item.index = n
	*q = append(*q, item)
}

func (q *Queue) Pop() interface{} {
	old := *q
	n := len(old)
	item := old[n-1]
	old[n-1] = nil  // avoid leak
	item.index = -1 // just in case
	*q = old[0 : n-1]
	return item
}

func TestPartOne(t *testing.T) {
	m, err := ReadMap("input.txt")
	if err != nil {
		t.Fatal(err)
	}

	q := make(Queue, 1)
	q[0] = &State{
		m:      m,
		energy: 0,
	}

	seen := make(map[string]bool)
	ans := 0
	var head *State
	for {
		head = heap.Pop(&q).(*State)

		s := head.m.String()
		if seen[s] {
			continue
		}
		seen[s] = true

		if head.m.Done() {
			ans = head.energy
			break
		}

		for _, m := range head.Moves() {
			heap.Push(&q, m)
		}
	}

	log.Println("Part 1:", ans)
}

func TestPartTwo(t *testing.T) {
	input, err := ReadMap("test.txt")
	if err != nil {
		t.Fatal(err)
	}

	_ = input

	ans := 0
	log.Println("Part 2:", ans)
}
