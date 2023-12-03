package solve

import (
	"log"
	"os"
	"testing"

	aoc "gitlab.com/vikblom/advent-of-code"
)

const FILE = "input.txt"

func readInput() (aoc.Matrix[int], error) {
	input, err := os.ReadFile(FILE)
	if err != nil {
		log.Fatal(err)
	}
	rows := 0
	data := []int{}
	for _, v := range input {
		if v == '\n' {
			rows++
		} else {
			data = append(data, int(v))
		}
	}

	return aoc.ToMatrix(data, rows, len(data)/rows), nil
}

func Step(karta aoc.Matrix[int]) {

	// >
	move := [][2]int{}
	for r := 0; r < karta.Height(); r++ {
		for c := 0; c < karta.Width(); c++ {
			if karta.At(r, c) != '>' {
				continue
			}
			next := (c + 1) % karta.Width()
			if karta.At(r, next) == '.' {
				move = append(move, [2]int{r, c})
			}
		}
	}
	for _, m := range move {
		r, c := m[0], m[1]
		next := (c + 1) % karta.Width()
		karta.Set(r, c, '.')
		karta.Set(r, next, '>')
	}

	// v
	move = [][2]int{}
	for r := 0; r < karta.Height(); r++ {
		for c := 0; c < karta.Width(); c++ {
			if karta.At(r, c) != 'v' {
				continue
			}
			next := (r + 1) % karta.Height()
			//fmt.Println(r, next)
			if karta.At(next, c) == '.' {
				move = append(move, [2]int{r, c})
			}
		}
	}
	for _, m := range move {
		r, c := m[0], m[1]
		next := (r + 1) % karta.Height()
		karta.Set(r, c, '.')
		karta.Set(next, c, 'v')
	}
}

func SolvePartOne(karta aoc.Matrix[int]) int {

	this := karta.String()
	steps := 0
	for {
		Step(karta)
		steps++

		next := karta.String()
		if this == next {
			break
		}
		this = next
	}
	return steps
}

func TestPartOne(t *testing.T) {
	karta, err := readInput()
	if err != nil {
		t.Fatal(err)
	}

	log.Println("Part 1:", SolvePartOne(karta))
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
