package solve

import (
	"bytes"
	"fmt"
	"log"
	"os"
	"testing"
)

// Pos forward, left, up.
type Pos struct {
	x, y, z int
}

type Scan []Pos

func Rotations(sc Scan) []Scan {

	rots := make([]Scan, 24)

	for _, p := range sc {
		// X and Z coordinates are given by "facing" and "up" respectively.
		// Y will be the RHS coordinate derived from those.

		// facing x and z is up
		rots[0] = append(rots[0], Pos{p.x, p.y, p.z})
		// facing x and -z is up
		rots[1] = append(rots[1], Pos{p.x, -p.y, -p.z})
		// facing x and y is up
		rots[2] = append(rots[2], Pos{p.x, -p.z, p.y})
		// facing x and -y is up
		rots[3] = append(rots[3], Pos{p.x, p.z, -p.y})

		// facing -x and z is up
		rots[4] = append(rots[4], Pos{-p.x, -p.y, p.z})
		// facing -x and -z is up
		rots[5] = append(rots[5], Pos{-p.x, p.y, -p.z})
		// facing -x and y is up
		rots[6] = append(rots[6], Pos{-p.x, p.z, p.y})
		// facing -x and -y is up
		rots[7] = append(rots[7], Pos{-p.x, -p.z, -p.y})

		// facing y and z is up
		rots[8] = append(rots[8], Pos{p.y, -p.x, p.z})
		// facing y and -z is up
		rots[9] = append(rots[9], Pos{p.y, p.x, -p.z})
		// facing y and x is up
		rots[10] = append(rots[10], Pos{p.y, p.z, p.x})
		// facing y and -x is up
		rots[11] = append(rots[11], Pos{p.y, -p.z, -p.x})

		// facing -y and z is up
		rots[12] = append(rots[12], Pos{-p.y, p.x, p.z})
		// facing -y and -z is up
		rots[13] = append(rots[13], Pos{-p.y, -p.x, -p.z})
		// facing -y and x is up
		rots[14] = append(rots[14], Pos{-p.y, -p.z, p.x})
		// facing -y and -x is up
		rots[15] = append(rots[15], Pos{-p.y, p.z, -p.x})

		// facing z and y is up
		rots[16] = append(rots[16], Pos{p.z, p.x, p.y})
		// facing z and -y is up
		rots[17] = append(rots[17], Pos{p.z, -p.x, -p.y})
		// facing z and x is up
		rots[18] = append(rots[18], Pos{p.z, -p.y, p.x})
		// facing z and -x is up
		rots[19] = append(rots[19], Pos{p.z, p.y, -p.x})

		// facing -z and y is up
		rots[20] = append(rots[20], Pos{-p.z, -p.x, p.y})
		// facing -z and -y is up
		rots[21] = append(rots[21], Pos{-p.z, p.x, -p.y})
		// facing -z and x is up
		rots[22] = append(rots[22], Pos{-p.z, p.y, p.x})
		// facing -z and -x is up
		rots[23] = append(rots[23], Pos{-p.z, -p.y, -p.x})
	}

	return rots
}

func TestRotations(t *testing.T) {
	sc := Scan{
		{0, 0, 0},
		{1, 2, 3},
	}
	rots := Rotations(sc)
	if len(rots) != 24 {
		t.Error("should always be 24 rotations")
	}
}

const FILE = "input.txt"

func readInput() ([]Scan, error) {
	input, err := os.ReadFile(FILE)
	if err != nil {
		log.Fatal(err)
	}

	scans := []Scan{}
	for _, c := range bytes.Split(input, []byte("\n\n")) {
		// Each scanner
		buf := bytes.NewBuffer(c)
		_, err := buf.ReadString('\n')
		if err != nil {
			return nil, err
		}

		sc := Scan{}
		for err == nil {
			var x, y, z int
			_, err = fmt.Fscanf(buf, "%d,%d,%d\n", &x, &y, &z)
			if err == nil {
				sc = append(sc, Pos{x, y, z})
			}
		}
		scans = append(scans, sc)
	}

	return scans, nil
}

func Overlap(a, b Scan) (bool, Pos) {

	seen := make(map[Pos]int)
	for _, aa := range a {
		for _, bb := range b {
			d := Pos{
				x: aa.x - bb.x,
				y: aa.y - bb.y,
				z: aa.z - bb.z,
			}
			seen[d]++
			if seen[d] == 12 {
				return true, d
			}
		}
	}

	return false, Pos{}
}

func TestOverlap(t *testing.T) {
	Overlap(Scan{{1, 1, 0}, {2, 2, 0}, {3, 1, 0}},
		Scan{{-1, 1, 0}, {-2, 2, 0}, {-3, 1, 0}})
}

func TestPartOne(t *testing.T) {
	scans, err := readInput()
	if err != nil {
		t.Fatal(err)
	}

	// Accumulate known beacon locations relative to scanner 0.
	// There is no need to track which scanner saw which beacon.
	known := scans[0]
	unknown := append([]Scan{}, scans[1:]...)

	for len(unknown) != 0 {
	try:
		for i, canidate := range unknown {
			for _, rr := range Rotations(canidate) {
				match, p := Overlap(known, rr)
				if match {
					//log.Println("match at", p)
					unknown = append(unknown[:i], unknown[i+1:]...)

					for _, pp := range rr {
						known = append(known, Pos{pp.x + p.x, pp.y + p.y, pp.z + p.z})
					}
					break try
				}
			}
		}
	}

	seen := make(map[Pos]bool)
	for _, p := range known {
		seen[p] = true
	}
	count := 0
	for range seen {
		count++
	}

	ans := count
	log.Println("Part 1:", ans)
}

func Abs(a int) int {
	if a < 0 {
		return -a
	}
	return a
}

func Manhattan(a, b Pos) int {
	return Abs(a.x-b.x) + Abs(a.y-b.y) + Abs(a.z-b.z)
}

func TestPartTwo(t *testing.T) {
	scans, err := readInput()
	if err != nil {
		t.Fatal(err)
	}

	// Accumulate known beacon locations relative to scanner 0.
	// There is no need to track which scanner saw which beacon.
	known := scans[0]
	unknown := append([]Scan{}, scans[1:]...)

	probes := []Pos{{0, 0, 0}}
	for len(unknown) != 0 {
	try:
		for i, canidate := range unknown {
			for _, rr := range Rotations(canidate) {
				match, p := Overlap(known, rr)
				if match {
					//log.Println("match at", p)
					unknown = append(unknown[:i], unknown[i+1:]...)

					for _, pp := range rr {
						known = append(known, Pos{pp.x + p.x, pp.y + p.y, pp.z + p.z})
					}
					probes = append(probes, p)
					break try
				}
			}
		}
	}

	longest := 0
	for i := range probes {
		for j := range probes {
			m := Manhattan(probes[i], probes[j])
			if m > longest {
				longest = m
			}
		}
	}

	ans := longest
	log.Println("Part 2:", ans)
}
