package solve

import (
	"container/heap"
	"log"
	"os"
	"testing"

	aoc "gitlab.com/vikblom/advent-of-code"
)

const FILE = "input.txt"

func readInput() (aoc.Matrix, error) {
	input, err := os.ReadFile(FILE)
	if err != nil {
		log.Fatal(err)
	}

	data := []int{}
	for _, r := range input {
		if r == '\n' {
			continue
		}
		data = append(data, int(r-'0'))
	}

	v := aoc.IntSqrt(len(data))
	return aoc.ToMatrix(data, v, v), nil
}

type Pos struct {
	x    int
	y    int
	risk int
	// The index is needed by update and is maintained by the heap.Interface methods.
	index int // The index of the item in the heap.
}

// PosQueue is a heap of Pos sorted by lowest risk.
type PosQueue []*Pos

func (pq PosQueue) Len() int           { return len(pq) }
func (pq PosQueue) Less(i, j int) bool { return pq[i].risk < pq[j].risk }

func (pq PosQueue) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
	pq[i].index = i
	pq[j].index = j
}

func (pq *PosQueue) Push(x interface{}) {
	n := len(*pq)
	item := x.(*Pos)
	item.index = n
	*pq = append(*pq, item)

}

func (pq *PosQueue) Pop() interface{} {
	old := *pq
	n := len(old)
	pos := old[n-1]
	old[n-1] = nil // avoid memory leak
	pos.index = -1 // for safety
	*pq = old[0 : n-1]
	return pos
}

// update modifies the priority and value of an Item in the queue.
func (pq *PosQueue) update(item *Pos, risk int) {
	item.risk = risk
	heap.Fix(pq, item.index)
}

func RiskAcross(cave aoc.Matrix) int {
	queue := make(PosQueue, 1)
	queue[0] = &Pos{
		x:    0,
		y:    0,
		risk: 0,
	}
	heap.Init(&queue)

	// Any position we have seen a path to will not be improved upon.
	seen := make(map[aoc.XY]*Pos)
	seen[aoc.XY{X: 0, Y: 0}] = queue[0]

	var at *Pos
	for {
		at = heap.Pop(&queue).(*Pos)
		if at.x == cave.Width()-1 && at.y == cave.Height()-1 {
			break
		}

		for _, nbr := range cave.Neighbours(at.x, at.y) {
			if _, ok := seen[nbr]; !ok {
				p := &Pos{
					x:    nbr.X,
					y:    nbr.Y,
					risk: at.risk + cave.At(nbr.X, nbr.Y),
				}
				heap.Push(&queue, p)
				seen[nbr] = p
			}
		}
	}
	return at.risk
}

func TestPartOne(t *testing.T) {
	cave, err := readInput()
	if err != nil {
		t.Fatal(err)
	}

	log.Println("Part 1:", RiskAcross(cave))
}

func Repeat(small aoc.Matrix, n int) aoc.Matrix {
	big := aoc.NewMatrix(n*small.Width(), n*small.Height())

	for across := 0; across < n; across++ {
		for down := 0; down < n; down++ {
			for x := 0; x < small.Width(); x++ {
				for y := 0; y < small.Width(); y++ {
					v := (small.At(x, y) + across + down) % 9
					if v == 0 {
						v = 9
					}
					big.Set(
						across*small.Width()+x,
						down*small.Height()+y,
						v)
				}
			}
		}
	}
	return big
}

func TestPartTwo(t *testing.T) {
	cave, err := readInput()
	if err != nil {
		t.Fatal(err)
	}

	bigCave := Repeat(cave, 5)
	//fmt.Println(bigCave.String())
	log.Println("Part 2:", RiskAcross(bigCave))
}
