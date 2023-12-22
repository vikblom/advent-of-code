package solve

import (
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

type pt3 struct {
	x, y, z int
}

func (p *pt3) hash() int {
	return p.x<<32 + p.y<<16 + p.z
}

type brick struct {
	start, end pt3
}

func parse(s string) pt3 {
	vs := strings.Split(s, ",")
	return pt3{
		x: aoc.MustInt(vs[0]),
		y: aoc.MustInt(vs[1]),
		z: aoc.MustInt(vs[2]),
	}
}

// Don't allocate.
var buf1 = make([]pt3, 0, 64)

func (b *brick) below() []pt3 {
	pts := buf1[:0]
	if min(b.start.z, b.end.z) == 1 {
		return pts
	}

	// Vertical
	if b.start.z < b.end.z || b.start == b.end {
		return append(pts, pt3{b.start.x, b.start.y, b.start.z - 1})
	}

	if b.start.x < b.end.x {
		for i := b.start.x; i <= b.end.x; i++ {
			pts = append(pts, pt3{
				x: i,
				y: b.start.y,
				z: b.start.z - 1,
			})
		}
		return pts
	}

	if b.start.y < b.end.y {
		for i := b.start.y; i <= b.end.y; i++ {
			pts = append(pts, pt3{
				x: b.start.x,
				y: i,
				z: b.start.z - 1,
			})
		}
		return pts
	}

	panic("nothing below")
}

func sign(i int) int {
	if i < 0 {
		return -1
	}
	if i > 0 {
		return 1
	}
	return 0
}

// Don't allocate.
var buf2 = make([]pt3, 0, 64)

func (b *brick) occupies() []pt3 {
	delta := pt3{
		sign(b.end.x - b.start.x),
		sign(b.end.y - b.start.y),
		sign(b.end.z - b.start.z),
	}
	at := b.start
	pts := append(buf2[:0], at)
	for at != b.end {
		at = pt3{
			at.x + delta.x,
			at.y + delta.y,
			at.z + delta.z,
		}
		pts = append(pts, at)
	}
	return pts
}

func TestPartOne(t *testing.T) {
	bricks := []brick{}
	for _, l := range inputLines {
		a, b, _ := strings.Cut(l, "~")
		bricks = append(bricks, brick{parse(a), parse(b)})
	}
	// Sort by increasing z, so just have to do 1
	// pass to see what drops down a level.
	slices.SortFunc(bricks, func(a, b brick) int {
		return a.start.z - b.start.z
	})

	// How big is the area looking from above.
	var w, d int
	for _, b := range bricks {
		w = max(w, b.end.x)
		d = max(d, b.end.y)
	}

	// Let them fall to rest.
	bricks = falling(bricks, w, d)

	ans := 0
	for i := range bricks {
		if !shaker(bricks, i, w, d) {
			ans += 1
		}
	}
	aoc.Answer(t, ans, 375)
}

func falling(bricks []brick, width, depth int) []brick {
	highest := aoc.NewMatrix[int](width+1, depth+1)
	for i := range bricks {
		// Drop while there is free space.
		for {
			haveSpace := true
			for _, pt := range bricks[i].below() {
				if pt.z <= highest.At(pt.x, pt.y) {
					haveSpace = false
					break
				}
			}
			if haveSpace && bricks[i].start.z > 1 {
				bricks[i].start.z -= 1
				bricks[i].end.z -= 1
			} else {
				break
			}
		}

		for _, pt := range bricks[i].occupies() {
			highest.Set(pt.x, pt.y, pt.z)
		}
	}

	return bricks
}

// shaker returns true if something moves when skip is removed.
func shaker(bricks []brick, skip, width, depth int) bool {
	highest := aoc.NewMatrix[int](width+1, depth+1)
	for i, b := range bricks {
		if i == skip {
			continue
		}
		allFree := true
		for _, pt := range b.below() {
			if pt.z <= highest.At(pt.x, pt.y) {
				allFree = false
				break
			}
		}
		if allFree && b.start.z > 1 {
			return true
		} else {
			for _, pt := range b.occupies() {
				highest.Set(pt.x, pt.y, pt.z)
			}
		}
	}
	return false
}

func TestPartTwo(t *testing.T) {
	bricks := []brick{}
	for _, l := range inputLines {
		a, b, _ := strings.Cut(l, "~")
		bricks = append(bricks, brick{parse(a), parse(b)})
	}
	// Sort by increasing z, so just have to do 1
	// pass to see what drops down a level.
	slices.SortFunc(bricks, func(a, b brick) int {
		return a.start.z - b.start.z
	})

	// How big is the area looking from above.
	var w, d int
	for _, b := range bricks {
		w = max(w, b.end.x)
		d = max(d, b.end.y)
	}

	// Let them fall to rest.
	bricks = falling(bricks, w, d)

	ans := 0
	for i := range bricks {
		ans += shakedown(bricks, i, w, d)
	}
	aoc.Answer(t, ans, 72352)
}

// shakedown counts how many bricks would move if skip is removed.
func shakedown(bricks []brick, skip, width, depth int) int {
	moved := 0
	highest := aoc.NewMatrix[int](width+1, depth+1)
	for i, b := range bricks {
		if i == skip {
			continue
		}
		allFree := true
		for _, pt := range b.below() {
			if pt.z <= highest.At(pt.x, pt.y) {
				allFree = false
				break
			}
		}
		if allFree && b.start.z > 1 {
			b.start.z -= 1
			b.end.z -= 1

			moved += 1
		} else {
			for _, pt := range b.occupies() {
				highest.Set(pt.x, pt.y, pt.z)
			}
		}
	}
	return moved
}
