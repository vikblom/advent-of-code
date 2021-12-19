package solve

import (
	"bufio"
	"bytes"
	"fmt"
	"log"
	"os"
	"strconv"
	"testing"
	"unicode"
)

const FILE = "input.txt"

func IsDigit(r byte) bool {
	return '0' <= r && r <= '9'
}

func TestExplode(t *testing.T) {
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
		{"[[6,[5,[11,[12,13]]]],14]", "[[6,[5,[23,0]]],27]"},
	}

	for _, tt := range tests {
		t.Log(tt.input)
		sf := []byte(tt.input)
		got := string(Explode(sf, FindExplosion(sf)))
		if tt.want != got {
			t.Fatalf("expected: %#v, got: %#v", tt.want, got)
		}
	}
}

func TestAddMany(t *testing.T) {
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

		got := AddMany(tt.input[0], tt.input[1:]...)
		if tt.want != got {
			t.Fatalf("\nexpected: %#v, \ngot:      %#v", tt.want, got)
		}
	}
}

func TestMagnitude(t *testing.T) {
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
		got := Magnitude(tt.input)
		if tt.want != got {
			t.Fatalf("expected: %#v, got: %#v", tt.want, got)
		}
	}
}

func FindExplosion(num []byte) int {
	depth := 0
	for i, v := range num {
		if v == '[' {
			depth++
		} else if v == ']' {
			depth--
		}
		if depth == 5 {
			return i
		}
	}
	return -1
}

// ExplodeRight from zero placed at n.
func ExplodeRight(sf []byte, n, add int) []byte {
	// First digit _after_ new '0'
	first := 0
	for i, v := range sf[n+1:] {
		if IsDigit(v) {
			first = n + 1 + i
			break
		}
	}
	if first == 0 {
		return sf
	}

	// The right number can possibly be more than one digit.
	len := 0
	for IsDigit(sf[first+len]) {
		len++
	}

	old, err := strconv.ParseInt(string(sf[first:first+len]), 10, 0)
	if err != nil {
		panic(err)
	}

	tail := append([]byte(fmt.Sprintf("%d", old+int64(add))), sf[first+len:]...)
	return append(sf[:first], tail...)
}

// ExplodeLeft from zero placed at n.
func ExplodeLeft(sf []byte, n, add int) []byte {

	// First digit _before_ new '0'
	first := 0
	for i := n - 1; i >= 0; i-- {
		if IsDigit(sf[i]) {
			first = i
			break
		}
	}
	if first == 0 {
		return sf
	}

	len := 1
	for IsDigit(sf[first-1]) {
		len++
		first--
	}

	old, err := strconv.ParseInt(string(sf[first:first+len]), 10, 0)
	if err != nil {
		panic(err)
	}

	tail := append([]byte(fmt.Sprintf("%d", old+int64(add))), sf[first+len:]...)
	return append(sf[:first], tail...)
}

func Explode(sf []byte, n int) []byte {
	top := n + 1
	for sf[top] != ']' {
		top++
	}
	top++ // include closing bracket
	var left, right int
	_, err := fmt.Sscanf(string(sf[n:top]), "[%d,%d]", &left, &right)
	if err != nil {
		panic(err)
	}
	//log.Println(left, right)

	// replace n:top with a '0'
	sf[n] = '0'
	copy(sf[n+1:], sf[top:])
	sf = sf[:len(sf)-(top-n-1)]

	//log.Println("zero ", string(sf))
	sf = ExplodeRight(sf, n, right)
	//log.Println("right", string(sf))
	sf = ExplodeLeft(sf, n, left)
	//log.Println("left ", string(sf))

	return sf
}

func readInput() ([]byte, error) {
	input, err := os.ReadFile(FILE)
	if err != nil {
		log.Fatal(err)
	}

	return input, nil
}

func FindSplit(sf []byte) int {
	for i := range sf {
		if IsDigit(sf[i]) && i < len(sf)-1 {
			if IsDigit(sf[i+1]) {
				return i
			}
		}
	}
	return -1
}

// Split the large int starting at n
func Split(sf []byte, n int) []byte {
	len := 0
	for IsDigit(sf[n+len]) {
		len++
	}

	old, err := strconv.ParseInt(string(sf[n:n+len]), 10, 0)
	if err != nil {
		panic(err)
	}
	a := old / 2
	b := old - a
	//log.Println(old, a, b)

	tail := sf[n+len:]
	tail = append([]byte(fmt.Sprintf("[%d,%d]", a, b)), sf[n+len:]...)

	return append(sf[:n], tail...)
}

func Add(left, right []byte) []byte {
	sf := make([]byte, len(left)+len(right)+3)
	sf = append([]byte{'['}, left...)
	sf = append(sf, ',')
	sf = append(sf, right...)
	sf = append(sf, ']')
	// log.Printf("after addition:  %s", string(sf))

	for {
		if n := FindExplosion(sf); n != -1 {
			sf = Explode(sf, n)
			// log.Printf("after explosion: %s", string(sf))
			continue
		}
		if n := FindSplit(sf); n != -1 {
			sf = Split(sf, n)
			// log.Printf("after split:     %s", string(sf))
			continue
		}
		break // Reduced
	}

	return sf
}

func AddMany(first string, rest ...string) string {
	sf := []byte(first)
	for _, other := range rest {
		sf = Add(sf, []byte(other))
		//log.Printf("ITER: %s", sf)
	}
	return string(sf)
}

func Magnitude(sf string) int {
	magnitude := 0
	prod := 1
	for _, r := range sf {
		if unicode.IsDigit(r) {
			magnitude += prod * int(r-'0')
		} else if r == '[' {
			prod *= 3
		} else if r == ',' {
			prod /= 3
			prod *= 2
		} else if r == ']' {
			prod /= 2
		}
	}
	return magnitude
}

func TestPartOne(t *testing.T) {
	input, err := readInput()
	if err != nil {
		t.Fatal(err)
	}
	sc := bufio.NewScanner(bytes.NewBuffer(input))

	lines := []string{}
	for sc.Scan() {
		lines = append(lines, sc.Text())
	}

	sf := AddMany(lines[0], lines[1:]...)

	ans := Magnitude(sf)
	log.Println("Part 1:", ans)
}

func TestPartTwo(t *testing.T) {
	input, err := readInput()
	if err != nil {
		t.Fatal(err)
	}
	sc := bufio.NewScanner(bytes.NewBuffer(input))

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

			if ans := Magnitude(AddMany(lines[i], lines[j])); ans > best {
				best = ans
			}
		}
	}
	log.Println("Part 2:", best)
}
