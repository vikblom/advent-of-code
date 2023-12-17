package aoc

import (
	"container/heap"
	"fmt"
	"strings"
)

type Queue[T any] struct {
	heap queue[T]
}

// If T had comparable we might be able to put it in a map?
// For update and such.
func NewQueue[T any]() *Queue[T] {
	q := &Queue[T]{
		heap: queue[T]{
			data: []qItem[T]{},
		},
	}
	heap.Init(&q.heap)
	return q
}

func (q Queue[T]) Len() int { return q.heap.Len() }

func (q *Queue[T]) Push(t T, score int) { heap.Push(&q.heap, qItem[T]{t: t, score: score}) }

func (q *Queue[T]) Pop() (T, int) {
	it := heap.Pop(&q.heap).(qItem[T])
	return it.t, it.score
}

// String debug representation, not guaranteed to be sorted.
func (q Queue[T]) String() string {
	var sb strings.Builder
	for _, t := range q.heap.data {
		fmt.Fprintf(&sb, "%v, ", t)
	}
	return sb.String()
}

type qItem[T any] struct {
	t     T
	score int
}

// queue implements heap.Interface.
type queue[T any] struct {
	data []qItem[T]
}

var _ heap.Interface = &queue[int]{}

func (pq queue[T]) Len() int           { return len(pq.data) }
func (pq queue[T]) Less(i, j int) bool { return pq.data[i].score < pq.data[j].score }

func (pq queue[T]) Swap(i, j int) {
	pq.data[i], pq.data[j] = pq.data[j], pq.data[i]
}

func (pq *queue[T]) Push(x interface{}) {
	item := x.(qItem[T])
	pq.data = append(pq.data, item)
}

func (pq *queue[T]) Pop() interface{} {
	old := pq.data
	n := len(old)
	out := old[n-1]
	old[n-1] = *new(qItem[T]) // avoid memory leak
	pq.data = old[0 : n-1]
	return out
}
