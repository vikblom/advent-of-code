package solve

import (
	"fmt"
	"slices"
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

func hash(s string) int {
	h := 0
	for _, b := range s {
		h += int(b)
		h *= 17
		h %= 256
	}
	return h
}

type Lens struct {
	label string
	focal int
}

func TestPartOne(t *testing.T) {

	ans := 0
	for _, s := range strings.Split(inputLines[0], ",") {
		ans += hash(s)
	}

	aoc.Answer(t, ans, 516657)
}

func TestPartTwo(t *testing.T) {
	boxes := [256][]Lens{}

	for _, s := range strings.Split(inputLines[0], ",") {
		var pop bool
		var label string
		var focal int
		if strings.HasSuffix(s, "-") {
			pop = true
			label, _, _ = strings.Cut(s, "-")
		} else {
			var rest string
			label, rest, _ = strings.Cut(s, "=")
			focal = aoc.MustInt(rest)
		}

		i := hash(label)
		if pop {
			at := slices.IndexFunc(boxes[i], func(l Lens) bool {
				return l.label == label
			})
			if at >= 0 {
				boxes[i] = append(boxes[i][:at], boxes[i][at+1:]...)
			}
		} else {
			at := slices.IndexFunc(boxes[i], func(l Lens) bool {
				return l.label == label
			})
			if at >= 0 {
				// Replace
				boxes[i][at].focal = focal
			} else {
				boxes[i] = append(boxes[i], Lens{label, focal})
			}
		}
	}

	ans := 0
	for i, box := range boxes {
		for j, l := range box {
			ans += (1 + i) * (1 + j) * l.focal
		}
	}
	aoc.Answer(t, ans, 210906)
}

func dump(boxes [256][]Lens) {
	for i, box := range boxes {
		if len(box) == 0 {
			continue
		}
		fmt.Printf("box %d: ", i)
		for _, l := range box {
			fmt.Printf("[%v %v] ", l.label, l.focal)
		}
		fmt.Println()
	}
	fmt.Println()
}
