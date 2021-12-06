package main

import (
	"log"
	"os"
)

func Part1(input []byte) (int, error) {
	return 0, nil
}

func Part2(input []byte) (int, error) {
	return 0, nil
}

func main() {
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
