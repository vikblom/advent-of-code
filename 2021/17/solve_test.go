package solve

import (
	"log"
	"math"
	"os"
	"testing"
)

const FILE = "input.txt"

func readInput() ([]byte, error) {
	input, err := os.ReadFile(FILE)
	if err != nil {
		log.Fatal(err)
	}

	return input, nil
}

type Target struct {
	xmin, xmax, ymin, ymax int
}

// Hits the zone
func (tgt *Target) Hits(x, y int) bool {
	return tgt.xmin <= x && x <= tgt.xmax && tgt.ymin <= y && y <= tgt.ymax
}

// Passes the zone eventually
func (tgt *Target) Passes(vx, vy int) int {

	var x, y, highest int

	for {
		x += vx
		y += vy

		if y > highest {
			highest = y
		}

		if vx > 0 {
			vx--
		} else if vx < 0 {
			vx++
		}
		vy--

		// log.Println(x, y, vx, vy)

		if tgt.Hits(x, y) {
			return highest
		}

		if x > tgt.xmax {
			return math.MinInt
		}

		if vy < 0 && y < tgt.ymin {
			return math.MinInt
		}

	}
	//return false
}

func TestPartOne(t *testing.T) {
	tgt := Target{70, 125, -159, -121}

	// Brute force the solution
	highest := 0
	for vy := 1000; vy > tgt.ymin; vy-- { // Decreasing
		for vx := 0; vx <= tgt.xmax; vx++ {
			if vx == 0 && vy == 0 {
				// fmt.Print("S")
			} else if tgt.Hits(vx, vy) {
				// fmt.Print("T")
			} else {
				h := tgt.Passes(vx, vy)
				if h != math.MinInt {
					// fmt.Print("x")
					if h > highest {
						highest = h
					}
				} else {
					// fmt.Print(".")
				}
			}
		}
		// fmt.Print("\n")
	}

	log.Println("Part 1:", highest)
}

func TestPartTwo(t *testing.T) {
	//tgt := Target{20, 30, -10, -5}
	tgt := Target{70, 125, -159, -121}

	// Brute force the solution
	count := 0
	for vy := 1000; vy >= tgt.ymin; vy-- { // Decreasing
		for vx := 0; vx <= tgt.xmax; vx++ {
			if tgt.Passes(vx, vy) != math.MinInt {
				//fmt.Printf("%d,%d\n", vx, vy)
				count++
			}

		}
	}

	log.Println("Part 2:", count)
}
