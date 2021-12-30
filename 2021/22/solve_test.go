package solve

import (
	"fmt"
	"log"
	"os"
	"testing"

	aoc "gitlab.com/vikblom/advent-of-code"
)

const FILE = "input.txt"

type Range struct {
	min, max int
}

type Cube struct {
	x, y, z Range
	op      bool
}

func readInput() ([]Cube, error) {
	fh, err := os.Open(FILE)
	if err != nil {
		return nil, err
	}
	defer fh.Close()

	steps := []Cube{}
	for err == nil {
		var s string
		st := Cube{}
		_, err = fmt.Fscanf(fh, "%s x=%d..%d,y=%d..%d,z=%d..%d",
			&s,
			&st.x.min, &st.x.max,
			&st.y.min, &st.y.max,
			&st.z.min, &st.z.max)
		st.op = s == "on"
		if err == nil {
			steps = append(steps, st)
		}
	}
	return steps, nil
}

func TestPartOne(t *testing.T) {
	steps, err := readInput()
	if err != nil {
		t.Fatal(err)
	}

	state := make(map[[3]int]bool)
	for _, st := range steps {

		if st.x.min < -50 || 50 < st.x.max {
			continue
		}
		if st.y.min < -50 || 50 < st.y.max {
			continue
		}
		if st.z.min < -50 || 50 < st.z.max {
			continue
		}

		for x := st.x.min; x <= st.x.max; x++ {
			for y := st.y.min; y <= st.y.max; y++ {
				for z := st.z.min; z <= st.z.max; z++ {
					state[[3]int{x, y, z}] = st.op
				}
			}
		}
	}

	ans := 0
	for _, v := range state {
		if v {
			ans++
		}
	}
	log.Println("Part 1:", ans)
}

func TestVolume(t *testing.T) {

	tests := []struct {
		input Cube
		want  int
	}{
		{
			input: Cube{x: Range{0, 0}, y: Range{0, 0}, z: Range{0, 0}},
			want:  1,
		},
		{
			input: Cube{x: Range{1, 10}, y: Range{1, 10}, z: Range{1, 10}},
			want:  10 * 10 * 10,
		},
		{
			input: Cube{x: Range{-4, 5}, y: Range{-4, 5}, z: Range{-4, 5}},
			want:  10 * 10 * 10,
		},
	}

	for _, tt := range tests {
		got := tt.input.Volume()
		if tt.want != got {
			t.Fatalf("expected: %#v, got: %#v", tt.want, got)
		}
	}

}

func (c Cube) Volume() int {
	return (c.x.max - c.x.min + 1) * (c.y.max - c.y.min + 1) * (c.z.max - c.z.min + 1)
}

func (c Cube) String() string {
	return fmt.Sprintf("[%t] %d..%d, %d..%d, %d..%d",
		c.op,
		c.x.min, c.x.max,
		c.y.min, c.y.max,
		c.z.min, c.z.max)
}

func TestIntersect(t *testing.T) {

	tests := []struct {
		first  Cube
		second Cube
		want   bool
		intsc  Cube
	}{
		{
			// No overlap in any dim
			first:  Cube{x: Range{0, 0}, y: Range{0, 0}, z: Range{0, 0}},
			second: Cube{x: Range{1, 1}, y: Range{1, 1}, z: Range{1, 1}},
			want:   false,
		},
		{
			// Overlap in only one dim
			first:  Cube{x: Range{0, 1}, y: Range{0, 0}, z: Range{0, 0}},
			second: Cube{x: Range{1, 1}, y: Range{1, 1}, z: Range{1, 1}},
			want:   false,
		},
		{
			// Overlap, turn of section
			first:  Cube{x: Range{0, 10}, y: Range{0, 10}, z: Range{0, 10}, op: true},
			second: Cube{x: Range{1, 1}, y: Range{1, 1}, z: Range{1, 1}, op: false},
			want:   true,
			intsc:  Cube{x: Range{1, 1}, y: Range{1, 1}, z: Range{1, 1}, op: false},
		},
	}

	for _, tt := range tests {
		got, cube := tt.first.Intersect(tt.second)
		if tt.want != got {
			t.Fatalf("expected: %#v, got: %#v", tt.want, got)
		}
		if tt.want {
			if tt.intsc != cube {
				t.Fatalf("expected: %s, got: %s", tt.intsc, cube)
			}
		}
	}

}

func (c Cube) Intersect(other Cube) (bool, Cube) {

	x1 := aoc.MaxInt(c.x.min, other.x.min)
	x2 := aoc.MinInt(c.x.max, other.x.max)
	y1 := aoc.MaxInt(c.y.min, other.y.min)
	y2 := aoc.MinInt(c.y.max, other.y.max)
	z1 := aoc.MaxInt(c.z.min, other.z.min)
	z2 := aoc.MinInt(c.z.max, other.z.max)

	if !(x1 <= x2 && y1 <= y2 && z1 <= z2) {
		return false, Cube{}
	}
	// If there is overlap we want to add new cubes
	// to correct the overall count.
	// What kind of cube depends on details of c and other.
	// If other is an "on" cube it will be added later by caller.

	var newOp bool
	if c.op == other.op {
		// for TT and FF we want to add opposite op
		// intersection to compensate since both cubes will
		// be in the accumulated list.
		newOp = !c.op
	} else {
		// else just let the other cube insert its state.
		newOp = other.op
	}

	return true, Cube{
		x:  Range{x1, x2},
		y:  Range{y1, y2},
		z:  Range{z1, z2},
		op: newOp}
}

func TestPartTwo(t *testing.T) {
	steps, err := readInput()
	if err != nil {
		t.Fatal(err)
	}

	// List of adjusted on/off cubes, in order or application.
	// In a perfect list this is just a bunch of disjoint "on" cubes.
	// But:
	// - If "on" cubes overlap we add an "off" of that intersection to correct.
	//   This will introduce "off" cubes into the list, and each "off" is caused
	//   by an earlier "on".
	// - "off" cubes should decrement the intersection from "on" cubes.
	// - When combining the two previous steps. "off" on "off" must add
	//   an "on" of their intersection to compensate for the earlier layers.
	state := []Cube{}
	for _, this := range steps {

		for _, already := range state {
			did, intsc := already.Intersect(this)
			if did {
				state = append(state, intsc)
			}
		}

		if this.op {
			state = append(state, this)
		}
	}

	ans := 0
	for _, c := range state {
		if c.op {
			ans += c.Volume()
		} else {
			ans -= c.Volume()
		}
	}
	log.Println("Part 2:", ans)
}
