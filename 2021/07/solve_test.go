package solve

import (
	"log"
	"testing"

	aoc "gitlab.com/vikblom/advent-of-code"
)

func FuelToPos(goal int, crabs map[int]int) int {
	fuel := 0
	for pos, n := range crabs {
		dist := pos - goal
		if dist < 0 {
			dist = -dist
		}
		fuel += n * dist
	}
	return fuel
}

func Part1(crabs []int) (int, error) {
	min := 9999
	max := -min
	atPos := make(map[int]int)
	for _, v := range crabs {
		if v < min {
			min = v
		}
		if v > max {
			max = v
		}
		atPos[v] += 1
	}

	if min == max {
		return 0, nil
	}

	bestFuel := 99999999
	for pos := min + 1; pos <= max; pos++ {
		f := FuelToPos(pos, atPos)
		if f < bestFuel {
			bestFuel = f
		}
	}

	return bestFuel, nil
}

func Part2(crabs []int) (int, error) {
	min := 9999
	max := -min
	atPos := make(map[int]int)
	for _, v := range crabs {
		if v < min {
			min = v
		}
		if v > max {
			max = v
		}
		atPos[v] += 1
	}

	if min == max {
		return 0, nil
	}

	lookup := make([]int, max-min+1)
	for i := 1; i < len(lookup); i++ {
		lookup[i] = lookup[i-1] + i
	}

	IncrFuelToPos := func(goal int, crabs map[int]int) int {
		fuel := 0
		for pos, n := range crabs {
			dist := pos - goal
			if dist < 0 {
				dist = -dist
			}
			fuel += n * lookup[dist]
		}
		return fuel
	}

	bestFuel := 99999999
	for pos := min + 1; pos <= max; pos++ {
		f := IncrFuelToPos(pos, atPos)
		if f < bestFuel {
			bestFuel = f
		}
	}

	return bestFuel, nil
}

func TestDay(t *testing.T) {
	crabs, err := aoc.ReadCSV("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	ans, err := Part1(crabs)
	if err != nil {
		log.Fatal(err)
	}
	log.Println("Part 1:", ans)

	ans, err = Part2(crabs)
	if err != nil {
		log.Fatal(err)
	}
	log.Println("Part 2:", ans)

}
