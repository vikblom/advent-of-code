package solve

import (
	"bufio"
	"bytes"
	"log"
	"os"
	"strconv"
	"strings"
	"testing"

	aoc "gitlab.com/vikblom/advent-of-code"
)

func Generation(fish []int) []int {

	new := 0
	for i, v := range fish {
		if v == 0 {
			fish[i] = 6
			new++
		} else {
			fish[i]--
		}
	}
	babies := make([]int, new)
	for i := range babies {
		babies[i] = 8
	}

	return append(fish, babies...)
}

func Part1(input []byte) (int, error) {
	r := bytes.NewBuffer(input)
	sc := bufio.NewScanner(r)
	sc.Split(aoc.ScanCSV)

	fish := []int{}
	for sc.Scan() {
		f, err := strconv.Atoi(strings.TrimSpace(sc.Text()))
		if err != nil {
			return 0, err
		}
		fish = append(fish, f)
	}

	for i := 0; i < 80; i++ {
		fish = Generation(fish)
	}

	return len(fish), nil
}

func Part2(input []byte) (int, error) {
	r := bytes.NewBuffer(input)
	sc := bufio.NewScanner(r)
	sc.Split(aoc.ScanCSV)

	fish := make([]int, 8+1)
	for sc.Scan() {
		f, err := strconv.Atoi(strings.TrimSpace(sc.Text()))
		if err != nil {
			return 0, err
		}
		fish[f]++
	}

	for i := 0; i < 256; i++ {
		fish = append(fish[1:], fish[0])
		fish[6] += fish[8]
	}

	sum := 0
	for _, v := range fish {
		sum += v
	}

	return sum, nil
}

func TestDay(t *testing.T) {
	input, err := os.ReadFile("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	ans, err := Part1(input)
	if err != nil {
		log.Fatal(err)
	}
	log.Println("Part 1:", ans)

	ans, err = Part2(input)
	if err != nil {
		log.Fatal(err)
	}
	log.Println("Part 2:", ans)

}
