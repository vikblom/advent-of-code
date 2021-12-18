package solve

import (
	"log"
	"os"
	"testing"
)

const FILE = "test.txt"

func readInput() ([]byte, error) {
	input, err := os.ReadFile(FILE)
	if err != nil {
		log.Fatal(err)
	}

	return input, nil
}

func TestPartOne(t *testing.T) {
	input, err := readInput()
	if err != nil {
		t.Fatal(err)
	}

	_ = input

	ans := 0
	log.Println("Part 1:", ans)
}

func TestPartTwo(t *testing.T) {
	input, err := readInput()
	if err != nil {
		t.Fatal(err)
	}

	_ = input

	ans := 0
	log.Println("Part 2:", ans)
}
