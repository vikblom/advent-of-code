package main

import (
	"bufio"
	"bytes"
	"fmt"
	"log"
	"os"
)

type Dot struct {
	x int
	y int
}

type Fold struct {
	dim string
	n   int
}

func DrawDots(dots map[Dot]bool) {
	var X, Y int
	for d := range dots {
		if d.x > X {
			X = d.x
		}
		if d.y > Y {
			Y = d.y
		}
	}

	for x := 0; x <= X; x++ {
		for y := 0; y <= Y; y++ {
			if dots[Dot{x, y}] {
				fmt.Print("#")
			} else {
				fmt.Print(".")
			}
		}
		fmt.Print("\n")
	}
	fmt.Print("\n")
}

func Part1(input []byte) (int, error) {
	sc := bufio.NewScanner(bytes.NewBuffer(input))

	// Dots - As a map to avoid duplicates
	dots := make(map[Dot]bool)
	for sc.Scan() {
		if sc.Text() == "" {
			break
		}
		var x, y int
		_, err := fmt.Sscanf(sc.Text(), "%d,%d", &x, &y)
		if err != nil {
			return 0, err
		}

		dots[Dot{x, y}] = true
	}

	// Folds
	folds := []Fold{}
	for sc.Scan() {
		var c rune
		var n int
		_, err := fmt.Sscanf(sc.Text(), "fold along %c=%d", &c, &n)
		if err != nil {
			return 0, err
		}
		folds = append(folds, Fold{string(c), n})
	}

	//DrawDots(dots)
	for _, fold := range folds {
		new := make(map[Dot]bool)
		for dot := range dots {
			d := Dot{dot.x, dot.y}
			if fold.dim == "x" {
				if d.x > fold.n {
					d.x = fold.n - (d.x - fold.n)
				}
			} else {
				if d.y > fold.n {
					d.y = fold.n - (d.y - fold.n)
				}
			}
			new[d] = true
		}
		dots = new
		//DrawDots(dots)
		break
	}

	count := 0
	for range dots {
		count++
	}

	return count, nil
}

func Part2(input []byte) (int, error) {
	sc := bufio.NewScanner(bytes.NewBuffer(input))

	// Dots - As a map to avoid duplicates
	dots := make(map[Dot]bool)
	for sc.Scan() {
		if sc.Text() == "" {
			break
		}
		var x, y int
		_, err := fmt.Sscanf(sc.Text(), "%d,%d", &x, &y)
		if err != nil {
			return 0, err
		}

		dots[Dot{x, y}] = true
	}

	// Folds
	folds := []Fold{}
	for sc.Scan() {
		var c rune
		var n int
		_, err := fmt.Sscanf(sc.Text(), "fold along %c=%d", &c, &n)
		if err != nil {
			return 0, err
		}
		folds = append(folds, Fold{string(c), n})
	}

	// Modify dots map inplace.
	for _, fold := range folds {
		for dot := range dots {
			if fold.dim == "x" {
				if dot.x > fold.n {
					delete(dots, dot)
					dot.x = fold.n - (dot.x - fold.n)
					dots[dot] = true
				}
			} else {
				if dot.y > fold.n {
					delete(dots, dot)
					dot.y = fold.n - (dot.y - fold.n)
					dots[dot] = true
				}
			}
		}
	}
	DrawDots(dots)

	count := 0
	for range dots {
		count++
	}

	return count, nil
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
