package solve

import (
	"bufio"
	"bytes"
	"log"
	"os"
	"sort"
	"testing"
)

var points = map[rune]int{
	')': 3,
	']': 57,
	'}': 1197,
	'>': 25137,
}

var match = map[rune]rune{
	'(': ')',
	'[': ']',
	'{': '}',
	'<': '>',
}

func Part1(input []byte) (int, error) {

	sum := 0
	sc := bufio.NewScanner(bytes.NewBuffer(input))
	for sc.Scan() {
		stack := []rune{}
		for _, r := range sc.Text() {
			_, opener := match[r]
			if opener {
				stack = append(stack, r)
			} else {
				expected := match[stack[len(stack)-1]]
				if r != expected {
					// Not matched, we get points
					sum += points[r]
					break
				} else {
					stack = stack[:len(stack)-1]
				}
			}
		}
	}

	return sum, nil
}

var points2 = map[rune]int{
	')': 1,
	']': 2,
	'}': 3,
	'>': 4,
}

func Part2(input []byte) (int, error) {

	scores := []int{}
	sc := bufio.NewScanner(bytes.NewBuffer(input))
outer:
	for sc.Scan() {
		stack := []rune{}
		for _, r := range sc.Text() {
			_, opener := match[r]
			if opener {
				stack = append(stack, r)
			} else {
				expected := match[stack[len(stack)-1]]
				if r != expected {
					// discard
					continue outer
				} else {
					stack = stack[:len(stack)-1]
				}
			}
		}
		// incomplete line
		score := 0
		for i := len(stack) - 1; i >= 0; i-- {
			score *= 5
			score += points2[match[stack[i]]]
		}
		scores = append(scores, score)
	}
	sort.Ints(scores)
	return scores[len(scores)/2], nil
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
