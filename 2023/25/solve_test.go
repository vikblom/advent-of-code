package solve

import (
	"fmt"
	"math/rand"
	"slices"
	"strings"
	"testing"

	aoc "gitlab.com/vikblom/advent-of-code"

	_ "embed"
)

var (
	//go:embed "input.txt"
	input      string
	inputLines = strings.Split(strings.TrimRight(input, "\n"), "\n")
)

type score struct {
	idx   int
	name  string
	count int
}

func TestPartOne(t *testing.T) {
	allLinks := map[string][]string{}
	for _, l := range inputLines {
		from, rest, _ := strings.Cut(l, ": ")
		rs := strings.Fields(rest)
		for _, to := range rs {
			allLinks[from] = append(allLinks[from], to)
			allLinks[to] = append(allLinks[to], from)
		}
	}

	// Map all nodes to a unique index.
	nodes := []string{}
	i := 0
	for from := range allLinks {
		nodes = append(nodes, from)
		i += 1
	}
	nodesIdx := map[string]int{}
	for i, n := range nodes {
		nodesIdx[n] = i
	}

	// Reachability matrix over indices.
	reachable := aoc.NewMatrix[bool](len(allLinks), len(allLinks))
	for from, tos := range allLinks {
		i := nodesIdx[from]
		for _, to := range tos {
			j := nodesIdx[to]
			reachable.Set(i, j, true)
		}
	}

	// Monte Carlo shortest path between random nodes.
	// The most common nodes on these paths should be the ones
	// we want to cut.
	counts := make([]int, len(nodes))
	for i := 0; i < 256; i++ {
		a, b := TwoRand(len(nodes))
		vs := ShortestPath(reachable, a, b)
		for _, v := range vs {
			counts[v] += 1
		}
	}

	scores := []score{}
	for i := 0; i < len(nodes); i++ {
		scores = append(scores, score{
			i,
			nodes[i],
			counts[i],
		})
	}
	slices.SortFunc(scores, func(a, b score) int {
		return b.count - a.count
	})

	// Cut the top 6 indiscriminately, we just want the group sizes.
	for _, i := range scores[:6] {
		// fmt.Println(i.name)
		for _, j := range scores[:6] {
			reachable.Set(i.idx, j.idx, false)
		}
	}

	// Finally, what is the size of the disjoint sub-groups?
	s := Size(reachable, 0)
	ans := s * (len(nodes) - s)
	aoc.Answer(t, ans, 527790)
}

func TwoRand(max int) (int, int) {
	a := rand.Intn(max)
	b := rand.Intn(max)
	return a, b
}

func Dump(nodes []string, reach aoc.Matrix[bool]) {
	for i := 0; i < reach.Rows; i++ {
		fmt.Printf("%s ", nodes[i])
		for j := 0; j < reach.Cols; j++ {
			if reach.At(i, j) {
				fmt.Printf("1 ")
			} else {
				fmt.Printf("_ ")
			}
		}
		fmt.Println()
	}
}

func Size(reachable aoc.Matrix[bool], start int) int {
	visited := make([]bool, reachable.Rows)
	visited[start] = true

	qs := make([]int, 1)
	qs[0] = start
	for len(qs) > 0 {
		q := qs[0]
		qs = qs[1:]

		for c := 0; c < reachable.Cols; c++ {
			if reachable.At(q, c) && !visited[c] {
				qs = append(qs, c)
				visited[c] = true
			}
		}
	}

	sum := 0
	for i, v := range visited {
		if v {
			sum += 1
		}
		visited[i] = false
	}
	return sum
}

type state struct {
	at   int
	path []int
}

func ShortestPath(reachable aoc.Matrix[bool], start, end int) []int {
	visited := make([]bool, reachable.Rows)
	visited[start] = true

	qs := []state{
		{at: start, path: []int{}},
	}
	for len(qs) > 0 {
		q := qs[0]
		qs = qs[1:]

		if q.at == end {
			return append(q.path, end)
		}

		for c := 0; c < reachable.Cols; c++ {
			if reachable.At(q.at, c) && !visited[c] {
				qs = append(qs, state{
					at:   c,
					path: append(slices.Clone(q.path), q.at),
				})
				visited[c] = true
			}
		}
	}
	panic("unreachable")
}
