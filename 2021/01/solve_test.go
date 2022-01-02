package solve

import (
	"log"
	"os"
	"testing"

	aoc "gitlab.com/vikblom/advent-of-code"
)

func countIncreases(data []int) int {

	prev := data[0]
	incr := 0
	for i := 1; i < len(data); i++ {
		if data[i] > prev {
			incr++
		}
		prev = data[i]
	}
	return incr
}

func countWindowIncrs(data []int) int {
	prev := data[0] + data[1] + data[2]
	incr := 0
	for i := 1; i < len(data)-2; i++ {
		x := data[i] + data[i+1] + data[i+2]
		if x > prev {
			incr++
		}
		prev = x
	}
	return incr
}

func TestDay(t *testing.T) {
	fh, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer fh.Close()

	data, err := aoc.ReadInts(fh)
	if err != nil {
		log.Fatal(err)
	}

	log.Println(countIncreases(data))
	log.Println(countWindowIncrs(data))
}
