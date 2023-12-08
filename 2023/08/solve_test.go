package solve

import (
	"strings"
	"testing"

	aoc "gitlab.com/vikblom/advent-of-code"

	_ "embed"
)

var (
	//go:embed "input.txt"
	input      string
	inputLines = strings.Split(strings.TrimRight(input, "\n"), "\n")
)

func TestPartOne(t *testing.T) {
	instr := inputLines[0]

	graph := map[string][]string{}
	for _, s := range inputLines[2:] {
		at, rest, _ := strings.Cut(s, " = ")
		l, r, _ := strings.Cut(rest[1:len(rest)-1], ", ")
		graph[at] = []string{l, r}
	}

	i := 0
	at := "AAA"
	for {
		if at == "ZZZ" {
			break
		}
		switch instr[i%len(instr)] {
		case 'L':
			at = graph[at][0]
		case 'R':
			at = graph[at][1]
		}
		i++
	}

	aoc.Answer(t, i, 12361)
}

// Greatest common divisor.
func gcd(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

// Least common multiplier.
func lcm(a int, bs ...int) int {
	for _, b := range bs {
		a = a * b / gcd(a, b)
	}
	return a
}

func TestPartTwo(t *testing.T) {
	instr := inputLines[0]

	ends := []string{}
	graph := map[string][]string{}
	for _, s := range inputLines[2:] {
		at, rest, _ := strings.Cut(s, " = ")
		l, r, _ := strings.Cut(rest[1:len(rest)-1], ", ")
		graph[at] = []string{l, r}
		if at[2] == 'Z' {
			ends = append(ends, at)
		}
	}

	// The problem decays into finding the loop for each *Z.
	// Each *Z loops back to itself, with a fixed number of steps.
	// Every *A arrives at its first *Z in the same number of steps
	// as that.
	loops := []int{}
	for _, e := range ends {
		at := e
		i := 0
		for {
			switch instr[i%len(instr)] {
			case 'L':
				at = graph[at][0]
			case 'R':
				at = graph[at][1]
			}
			i++
			if at == e {
				loops = append(loops, i)
				break
			}
		}
	}

	ans := lcm(loops[0], loops[1:]...)

	aoc.Answer(t, ans, 18215611419223)
}
