package main

import (
	"bufio"
	"bytes"
	"log"
	"os"
	"sort"
)

type Readout struct {
	signals []string
	digits  []string
}

func OfLength(n int, ss []string) int {
	count := 0
	for _, s := range ss {
		if len(s) == n {
			count++
		}
	}
	return count
}

func Part1(readouts []*Readout) (int, error) {
	var ones, fours, sevens, eights int
	for _, ro := range readouts {
		ones += OfLength(2, ro.digits)
		fours += OfLength(4, ro.digits)
		sevens += OfLength(3, ro.digits)
		eights += OfLength(7, ro.digits)
	}
	return ones + fours + sevens + eights, nil
}

func FirstOfLength(n int, ss []string) string {
	for _, s := range ss {
		if len(s) == n {
			return s
		}
	}
	panic("Unreachable")
}

func Overlap(s1, s2 string) int {
	m := map[rune]int{}
	for _, c := range s1 {
		m[c] = 1
	}
	count := 0
	for _, c := range s2 {
		if _, ok := m[c]; ok {
			count++
		}
	}
	return count
}

func SortString(s string) string {
	r := []rune(s)
	sort.Slice(r, func(i int, j int) bool { return r[i] < r[j] })
	return string(r)
}

func Part2(readouts []*Readout) (int, error) {

	sum := 0
	for _, ro := range readouts {
		key := map[string]int{}
		one := FirstOfLength(2, ro.signals)
		four := FirstOfLength(4, ro.signals)

		key[one] = 1
		key[four] = 4
		key[FirstOfLength(3, ro.signals)] = 7
		key[FirstOfLength(7, ro.signals)] = 8

		for _, s := range ro.signals {
			if _, ok := key[s]; ok {
				continue
			}
			if len(s) == 5 {
				if Overlap(s, one) == 2 {
					key[s] = 3
				} else if Overlap(s, four) == 3 {
					key[s] = 5
				} else {
					key[s] = 2
				}
			} else if len(s) == 6 {
				if Overlap(s, one) == 1 {
					key[s] = 6
				} else if Overlap(s, four) == 4 {
					key[s] = 9
				} else {
					key[s] = 0
				}
			} else {
				println("Unexpected length")
			}
		}

		// Annoyingly, digits can reorder things.
		for k, v := range key {
			key[SortString(k)] = v
		}

		val := 0
		for _, d := range ro.digits {
			v := key[SortString(d)]
			val = 10*val + v
		}
		sum += val
	}

	return sum, nil
}

func ParseReadouts(input []byte) ([]*Readout, error) {
	r := bytes.NewBuffer(input)
	sc := bufio.NewScanner(r)
	sc.Split(bufio.ScanWords)

	readouts := make([]*Readout, 0)
parse:
	for sc.Err() == nil {
		ro := &Readout{
			signals: []string{},
			digits:  []string{},
		}
		for i := 0; i < 10; i++ {
			if !sc.Scan() {
				break parse
			}
			ro.signals = append(ro.signals, sc.Text())
		}
		sc.Scan()
		for i := 0; i < 4; i++ {
			sc.Scan()
			ro.digits = append(ro.digits, sc.Text())
		}
		readouts = append(readouts, ro)
	}
	return readouts, nil
}

func main() {
	input, err := os.ReadFile("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	readouts, err := ParseReadouts(input)
	if err != nil {
		log.Fatal(err)
	}

	ans, err := Part1(readouts)
	if err != nil {
		log.Fatal(err)
	}
	log.Println("Part 1:", ans)

	ans, err = Part2(readouts)
	if err != nil {
		log.Fatal(err)
	}
	log.Println("Part 2:", ans)

}
