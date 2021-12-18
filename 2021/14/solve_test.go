package solve

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"testing"
)

const FILE = "input.txt"

type RunePair struct {
	left  rune
	right rune
}

func readInput() ([]rune, map[RunePair]rune, error) {
	fh, err := os.Open(FILE)
	if err != nil {
		return nil, nil, err
	}
	sc := bufio.NewScanner(fh)

	// Polymer
	sc.Scan()
	polymer := []rune(sc.Text())
	sc.Scan() // empty line

	// Rules
	rules := make(map[RunePair]rune)
	for sc.Scan() {
		var l, r, c rune
		_, err := fmt.Sscanf(sc.Text(), "%c%c -> %c", &l, &r, &c)
		if err != nil {
			return nil, nil, err
		}
		rules[RunePair{l, r}] = c
	}

	return polymer, rules, nil
}

func TestPartOne(t *testing.T) {
	polymer, rules, err := readInput()
	if err != nil {
		t.Fatal(err)
	}

	for iter := 0; iter < 10; iter++ {
		next := []rune{}
		for i := range polymer {
			next = append(next, polymer[i])
			if i == len(polymer)-1 {
				break
			}

			c, ok := rules[RunePair{polymer[i], polymer[i+1]}]
			if ok {
				next = append(next, c)
			}
		}
		polymer = next
	}

	counts := make(map[rune]int)
	for _, r := range polymer {
		counts[r]++
	}

	//log.Println(counts)

	most := 0
	least := 1 << 32
	for _, c := range counts {
		if c > most {
			most = c
		}
		if c < least {
			least = c
		}
	}

	ans := most - least
	log.Println("Part 1:", ans)
}

type Input struct {
	left rune
	righ rune
	n    int
}

type PolymerExpander struct {
	rules  map[RunePair]rune
	memory map[Input]map[rune]int
}

func (pe *PolymerExpander) Expand(left, right rune, n int) (counts map[rune]int) {
	//log.Println(string(left), string(right))

	// Already did this input?
	counts, ok := pe.memory[Input{left, right, n}]
	if ok {
		return
	}

	// Else we have to calculate
	counts = make(map[rune]int)
	c, ok := pe.rules[RunePair{left, right}]
	if !ok || n == 0 {
		// Will not expand more, since pairs overlap, make sure to not count one char twice.
		counts[right] += 1
	} else {
		for rune, count := range pe.Expand(left, c, n-1) {
			counts[rune] = count
		}
		for rune, count := range pe.Expand(c, right, n-1) {
			counts[rune] += count
		}
	}
	// Memoize
	pe.memory[Input{left, right, n}] = counts
	return
}

func TestPartTwo(t *testing.T) {
	polymer, rules, err := readInput()
	if err != nil {
		t.Fatal(err)
	}

	pe := PolymerExpander{
		rules:  rules,
		memory: make(map[Input]map[rune]int),
	}
	counts := make(map[rune]int)
	for i := 0; i < len(polymer)-1; i++ {
		for char, count := range pe.Expand(polymer[i], polymer[i+1], 40) {
			counts[char] += count
		}
	}
	// Patch up counts after only counting "rights".
	counts[polymer[0]]++

	//log.Println(counts)

	most := 0
	least := math.MaxInt64
	for _, c := range counts {
		if c > most {
			most = c
		}
		if c < least && c != 0 {
			least = c
		}
	}

	ans := most - least
	log.Println("Part 2:", ans)
}
