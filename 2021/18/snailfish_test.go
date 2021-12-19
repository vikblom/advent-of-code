package solve_test

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
	"testing"
	"unicode"
)

type Node struct {
	val   int
	depth int
}

type Snailfish []Node

func ParseSnailfish(input string) Snailfish {
	sf := Snailfish{}
	depth := 0
	for _, r := range input {
		switch r {
		case '[':
			depth++
		case ']':
			depth--
		case ',':
			//ignore
		default:
			sf = append(sf, Node{val: int(r - '0'), depth: depth})
		}
	}
	return sf
}

func (sf Snailfish) String() string {
	var s strings.Builder

	// Keep track of the nested levels.
	// True means this level already has its left side.
	stack := []bool{}
	for _, e := range sf {
		for len(stack) < e.depth {
			fmt.Fprint(&s, "[")
			stack = append(stack, false)
		}

		// We write a value and then ] while the top of the stack is true.
		// Since each write is completing a pair, another symbol is needed.
		// If hitting false on the stack, we have written the left side of a pair,
		// therefore "," is written, the top of the stack is changed, and we move on.
		fmt.Fprintf(&s, "%d", e.val)
		for stack[len(stack)-1] {
			fmt.Fprint(&s, "]")
			stack = stack[:len(stack)-1]
			if len(stack) == 0 {
				return s.String()
			}
		}
		fmt.Fprint(&s, ",")
		stack[len(stack)-1] = true
	}
	return s.String()
}

func (sf *Snailfish) Explode() bool {
	for i := range *sf {
		if (*sf)[i].depth > 4 {
			// Transfer values
			left := i
			if 0 < left {
				(*sf)[left-1].val += (*sf)[left].val
			}
			right := i + 1
			if right < len(*sf)-1 {
				(*sf)[right+1].val += (*sf)[right].val
			}
			// Cut out exploded
			(*sf)[left] = Node{val: 0, depth: 4}
			*sf = append((*sf)[:right], (*sf)[right+1:]...)
			return true
		}
	}
	return false
}

func (sf *Snailfish) Split() bool {
	for i := range *sf {
		if (*sf)[i].val > 9 {
			// Transfer values
			d := (*sf)[i].depth + 1
			a := Node{
				val:   (*sf)[i].val / 2,
				depth: d,
			}
			b := Node{
				val:   (*sf)[i].val - a.val,
				depth: d,
			}

			// Insert split
			*sf = append((*sf)[:i], append([]Node{a, b}, (*sf)[i+1:]...)...)
			return true
		}
	}
	return false
}

func (sf *Snailfish) Add(other Snailfish) {
	*sf = append(*sf, other...)
	for i := range *sf {
		(*sf)[i].depth++
	}

	for {
		if sf.Explode() {
			continue
		}
		if sf.Split() {
			continue
		}
		break
	}
}

func (sf Snailfish) Magnitude() int {
	// Keep track of the nested levels.
	// True means we have already seen the left side.
	mag := 0
	prod := 1
	for _, r := range sf.String() {
		if unicode.IsDigit(r) {
			mag += prod * int(r-'0')
		} else if r == '[' {
			prod *= 3
		} else if r == ',' {
			prod /= 3
			prod *= 2
		} else if r == ']' {
			prod /= 2
		}

	}
	return mag
}

func TestSnailfishExplode(t *testing.T) {
	tests := []struct {
		input string
		want  string
	}{
		{"[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]"},
		{"[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]"},
		{"[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]"},
		{"[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"},
		{"[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"},
		{"[[[[0,7],4],[7,[[8,4],9]]],[1,1]]", "[[[[0,7],4],[15,[0,13]]],[1,1]]"},
	}

	for _, tt := range tests {
		//t.Log(tt.input)
		sf := ParseSnailfish(tt.input)
		sf.Explode()
		got := sf.String()
		if tt.want != got {
			t.Fatalf("expected: %#v, got: %#v", tt.want, got)
		}
	}
}

func TestParseSnailfish(t *testing.T) {
	tests := []struct {
		input string
	}{
		{"[1,2]"},
		{"[[1,2],3]"},
		{"[9,[8,7]]"},
		{"[[1,9],[8,5]]"},
		{"[[[[1,2],[3,4]],[[5,6],[7,8]]],9]"},
		{"[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]"},
		{"[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"},
	}

	for _, tt := range tests {

		sf := ParseSnailfish(tt.input)

		got := sf.String()
		if tt.input != got {
			t.Fatalf("\nexpected: %#v, \n     got: %#v", tt.input, got)
		}
	}
}

func TestSnailfishAdd(t *testing.T) {
	tests := []struct {
		input []string
		want  string
	}{
		{
			input: []string{"[1,1]", "[2,2]", "[3,3]", "[4,4]"},
			want:  "[[[[1,1],[2,2]],[3,3]],[4,4]]",
		},
		{
			input: []string{"[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]"},
			want:  "[[[[3,0],[5,3]],[4,4]],[5,5]]",
		},
		{
			input: []string{"[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]", "[6,6]"},
			want:  "[[[[5,0],[7,4]],[5,5]],[6,6]]",
		},
		{
			input: []string{
				"[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
				"[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
				"[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
				"[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]",
				"[7,[5,[[3,8],[1,4]]]]",
				"[[2,[2,2]],[8,[8,1]]]",
				"[2,9]",
				"[1,[[[9,3],9],[[9,0],[0,7]]]]",
				"[[[5,[7,4]],7],1]",
				"[[[[4,2],2],6],[8,7]]",
			},
			want: "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]",
		},
	}

	for _, tt := range tests {
		sf := ParseSnailfish(tt.input[0])
		for _, s := range tt.input[1:] {
			a := ParseSnailfish(s)
			sf.Add(a)
		}
		got := sf.String()
		if tt.want != got {
			t.Fatalf("\nexpected: %#v, \ngot:      %#v", tt.want, got)
		}
	}
}

func TestSnailfishMagnitude(t *testing.T) {
	tests := []struct {
		input string
		want  int
	}{
		{"[[1,2],[[3,4],5]]", 143},
		{"[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", 1384},
		{"[[[[1,1],[2,2]],[3,3]],[4,4]]", 445},
		{"[[[[3,0],[5,3]],[4,4]],[5,5]]", 791},
		{"[[[[5,0],[7,4]],[5,5]],[6,6]]", 1137},
		{"[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", 3488},
	}

	for _, tt := range tests {
		got := ParseSnailfish(tt.input).Magnitude()
		if tt.want != got {
			t.Fatalf("expected: %#v, got: %#v", tt.want, got)
		}
	}
}

func TestSnailfishPartOne(t *testing.T) {
	fh, err := os.Open("input.txt")
	if err != nil {
		t.Fatal(err)
	}
	defer fh.Close()
	sc := bufio.NewScanner(fh)

	lines := []string{}
	for sc.Scan() {
		lines = append(lines, sc.Text())
	}

	sf := ParseSnailfish(lines[0])
	for _, l := range lines[1:] {
		sf.Add(ParseSnailfish(l))
	}

	log.Println("Part 1:", sf.Magnitude())
}

func TestSnailfishPartTwo(t *testing.T) {
	fh, err := os.Open("input.txt")
	if err != nil {
		t.Fatal(err)
	}
	defer fh.Close()
	sc := bufio.NewScanner(fh)

	lines := []string{}
	for sc.Scan() {
		lines = append(lines, sc.Text())
	}

	best := 0
	for i := 0; i < len(lines); i++ {
		for j := 0; j < len(lines); j++ {
			if i == j {
				continue
			}

			sf := ParseSnailfish(lines[i])
			sf.Add(ParseSnailfish(lines[j]))
			mag := sf.Magnitude()
			if best < mag {
				best = mag
			}
		}
	}
	log.Println("Part 2:", best)
}
