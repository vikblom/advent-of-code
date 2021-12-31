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

var energy = map[int]int{
	'A': 1,
	'B': 10,
	'C': 100,
	'D': 1000,
}

var homecol = map[int]int{
	'A': 3,
	'B': 5,
	'C': 7,
	'D': 9,
}

type Map struct {
	rows int
	cols int
	data []int // row major
}

type Pods map[[2]int]int

func ReadMap(file string, height, width int) (Map, Pods, error) {
	fh, err := os.Open(file)
	if err != nil {
		log.Fatal(err)
	}
	defer fh.Close()
	buf := bufio.NewReader(fh)

	pods := make(Pods)
	mat := NewMap(height, width)
	row := 0
	col := 0
	for err == nil {
		b, err := buf.ReadByte()
		if err != nil {
			if err == io.EOF {
				break
			} else {
				return mat, pods, err
			}
		}

		if b == '\n' {
			col = 0
			row++
		} else {
			if 'A' <= b && b <= 'Z' {
				pods[[2]int{row, col}] = int(b)
				mat.Set(row, col, '.')
			} else {
				mat.Set(row, col, int(b))
			}
			col++
		}
	}
	return mat, pods, nil
}

func NewMap(rows, cols int) Map {
	return Map{
		rows: rows,
		cols: cols,
		data: make([]int, rows*cols),
	}
}

func Draw(m Map, p Pods) string {
	var s strings.Builder
	for row := 0; row < m.rows; row++ {
		for col := 0; col < m.cols; col++ {
			if c, ok := p[[2]int{row, col}]; ok {
				fmt.Fprintf(&s, "%c", c)
			} else {
				c := m.At(row, col)
				if c > 0 {
					fmt.Fprintf(&s, "%c", c)
				}
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

func (p Pods) Done() bool {
	for k, v := range p {
		row := k[0]
		col := k[1]
		if row < 2 {
			return false
		}
		if col != homecol[v] {
			return false
		}
	}
	return true
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

func (p Pods) Copy() Pods {
	new := make(Pods)
	for k, v := range p {
		new[k] = v
	}
	return new
}

func (s State) AllMovesFrom(m Map, row, col int) []*State {
	c := s.pods[[2]int{row, col}]
	if row == 1 { // corridor
		home := homecol[c]
		// There must be place to move in
		if _, busy := s.pods[[2]int{2, home}]; busy {
			return nil
		}
		// Can only move in with friends or empty
		nbr, busy := s.pods[[2]int{3, home}]
		if busy && nbr != c {
			return nil
		}
		// Check if the corridor is free, note that no ant can be at
		// the opening, so checking (left, right) is fine.
		left := aoc.MinInt(col, home)
		right := aoc.MaxInt(col, home)
		clear := true
		for i := left + 1; i < right; i++ {
			if _, busy := s.pods[[2]int{1, i}]; busy {
				clear = false
				break
			}
		}
		if !clear {
			return nil
		}
		// Actually move, figure out how deep we should place us.
		d := 2
		for s.pods[[2]int{d + 1, home}] == 0 && d < m.rows-2 {
			d++
		}

		e := energy[c] * (aoc.AbsInt(col-home) + aoc.AbsInt(row-d))
		next := s.pods.Copy()
		delete(next, [2]int{row, col})
		next[[2]int{d, home}] = c
		return []*State{
			{
				energy: s.energy + e,
				pods:   next,
			},
		}

	} else {
		// Check if column is already packed with correct ants.
		if col == homecol[c] {
			if row == 3 || s.pods[[2]int{3, col}] == c {
				//log.Println("ALREADY GOOD")
				return nil
			}
		}

		// Check if this and is blocked in by some other ant.
		if row == 3 && s.pods[[2]int{2, col}] > 0 {
			// Some other ant must get out first.
			//log.Println("LOCKED IN")
			return nil
		}

		// All possible moves into corridor
		left := col
		for s.pods[[2]int{1, left}] == 0 && left > 0 {
			left--
		}
		right := col
		for s.pods[[2]int{1, right}] == 0 && right < m.cols-1 {
			right++
		}

		news := []*State{}
		for i := left + 1; i < right; i++ {
			if i == 3 || i == 5 || i == 7 || i == 9 {
				continue // cannot block entrances
			}
			e := energy[c] * (aoc.AbsInt(col-i) + aoc.AbsInt(row-1))
			next := s.pods.Copy()
			delete(next, [2]int{row, col})
			next[[2]int{1, i}] = c
			news = append(news, &State{
				energy: s.energy + e,
				pods:   next,
			})
		}
		return news
	}
}

type State struct {
	pods   Pods
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

// func TestFoo(t *testing.T) {
// 	m, pods, err := ReadMap("foo.txt", 5, 13)
// 	if err != nil {
// 		t.Fatal(err)
// 	}

// 	head := State{
// 		energy: 0,
// 		pods:   pods,
// 	}

// 	//for pos := range pods {
// 	//for _, st := range head.AllMovesFrom(m, pos[0], pos[1]) {
// 	for _, st := range head.AllMovesFrom(m, 1, 11) {
// 		fmt.Println(Draw(m, st.pods))
// 	}
// 	//}
// }

func TestPartOne(t *testing.T) {
	m, pods, err := ReadMap("input.txt", 5, 13)
	if err != nil {
		t.Fatal(err)
	}

	q := make(Queue, 1)
	q[0] = &State{
		pods:   pods,
		energy: 0,
	}

	seen := make(map[string]bool)
	ans := 0
	var head *State
	for {
		head = heap.Pop(&q).(*State)

		str := Draw(m, head.pods)
		if seen[str] {
			continue
		}
		seen[str] = true

		if head.pods.Done() {
			ans = head.energy
			break
		}

		for pos := range head.pods {
			for _, st := range head.AllMovesFrom(m, pos[0], pos[1]) {
				heap.Push(&q, st)
			}
		}
	}

	log.Println("Part 1:", ans)
}

func TestPartTwo(t *testing.T) {
	input, pods, err := ReadMap("test2.txt", 7, 13)
	if err != nil {
		t.Fatal(err)
	}

	_ = pods
	_ = input

	ans := 0
	log.Println("Part 2:", ans)
}
