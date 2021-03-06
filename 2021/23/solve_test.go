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
	"time"

	aoc "gitlab.com/vikblom/advent-of-code"
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

func ReadMap(file string, height, width int) (Map, error) {
	fh, err := os.Open(file)
	if err != nil {
		log.Fatal(err)
	}
	defer fh.Close()
	buf := bufio.NewReader(fh)

	mat := NewMap(height, width)
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

// CanGoHome only if moving in with friends/empty and corridor is unobstructed.
func (m Map) CanGoHome(row, col int) bool {
	c := m.At(row, col)
	home := homecol[rune(c)]
	for d := m.rows - 2; d >= 2; d-- {
		o := m.At(d, home)
		if o == '.' {
			break
		} else if o != c {
			return false
		}
	}

	// Check if the corridor is free, note that no ant can be at
	// the opening, so checking (left, right) is fine.
	left := aoc.MinInt(col, home)
	right := aoc.MaxInt(col, home)
	for i := left + 1; i < right; i++ {
		if m.At(1, i) != '.' {
			return false
		}
	}

	return true
}

// AlreadyHome in the correct position.
func (m Map) AlreadyHome(row, col int) bool {
	c := m.At(row, col)
	home := homecol[rune(c)]
	if col != home {
		return false
	}
	for d := row + 1; d < m.rows-1; d++ {
		if m.At(d, home) != c {
			return false
		}
	}
	return true
}

// Blocked in this sideroom for now.
func (m Map) Blocked(row, col int) bool {
	// Check if its even possible to move out
	for d := 2; d < row; d++ {
		if m.At(d, col) != '.' {
			return true
		}
	}
	return false
}

func (s State) AllMovesFrom(row, col int) []*State {
	c := s.m.At(row, col)
	home := homecol[rune(c)]
	if row == 1 { // corridor

		if !s.m.CanGoHome(row, col) {
			return nil
		}

		// Actually move, figure out how deep.
		d := 2
		for s.m.At(d+1, home) == '.' && d < s.m.rows-2 {
			d++
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
		if s.m.AlreadyHome(row, col) {
			return nil
		}

		if s.m.Blocked(row, col) {
			return nil
		}

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
	// unreachable
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

func Solve(m Map) int {
	q := make(Queue, 1)
	q[0] = &State{
		m:      m,
		energy: 0,
	}

	seen := make(map[string]bool)
	var head *State
	for len(q) > 0 {
		head = heap.Pop(&q).(*State)

		s := head.m.String()
		if seen[s] {
			continue
		}
		seen[s] = true

		if head.m.Done() {
			return head.energy
		}

		for _, next := range head.Moves() {
			heap.Push(&q, next)
		}
	}
	return -1
}

// func TestFoo(t *testing.T) {
// 	m, err := ReadMap("foo.txt", 5, 13)
// 	if err != nil {
// 		t.Fatal(err)
// 	}
// 	fmt.Println(m)
// 	fmt.Println()

// 	st := State{m: m, energy: 0}
// 	for _, next := range st.Moves() {
// 		fmt.Println(next.m)
// 		fmt.Println()
// 	}
// }

func TestPartOne(t *testing.T) {
	m, err := ReadMap("test.txt", 5, 13)
	if err != nil {
		t.Fatal(err)
	}
	start := time.Now()
	log.Println("Test 1:", Solve(m), time.Since(start))

	m, err = ReadMap("input.txt", 5, 13)
	if err != nil {
		t.Fatal(err)
	}
	start = time.Now()
	log.Println("Part 1:", Solve(m), time.Since(start))
}

func TestPartTwo(t *testing.T) {
	m, err := ReadMap("test2.txt", 7, 13)
	if err != nil {
		t.Fatal(err)
	}
	start := time.Now()
	log.Println("Test 2:", Solve(m), time.Since(start))

	m, err = ReadMap("input2.txt", 7, 13)
	if err != nil {
		t.Fatal(err)
	}
	start = time.Now()
	log.Println("Part 2:", Solve(m), time.Since(start))
}
