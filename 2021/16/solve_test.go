package solve

import (
	"fmt"
	"log"
	"math"
	"os"
	"strings"
	"testing"
)

const FILE = "input.txt"

var conv = map[rune][]int{
	'0': {0, 0, 0, 0},
	'1': {0, 0, 0, 1},
	'2': {0, 0, 1, 0},
	'3': {0, 0, 1, 1},
	'4': {0, 1, 0, 0},
	'5': {0, 1, 0, 1},
	'6': {0, 1, 1, 0},
	'7': {0, 1, 1, 1},
	'8': {1, 0, 0, 0},
	'9': {1, 0, 0, 1},
	'A': {1, 0, 1, 0},
	'B': {1, 0, 1, 1},
	'C': {1, 1, 0, 0},
	'D': {1, 1, 0, 1},
	'E': {1, 1, 1, 0},
	'F': {1, 1, 1, 1},
}

func toString(ints []int) string {
	var s strings.Builder
	for _, r := range ints {
		if r == 0 {
			fmt.Fprint(&s, "0")
		} else {
			fmt.Fprint(&s, "1")
		}
	}
	return s.String()
}

func decode(hex string) []int {
	out := []int{}
	for _, r := range hex {
		out = append(out, conv[r]...)
	}
	return out
}

func TestDecode(t *testing.T) {

	tests := []struct {
		input string
		want  string
	}{
		{"D2FE28", "110100101111111000101000"},
		{"38006F45291200", "00111000000000000110111101000101001010010001001000000000"},
	}

	for _, tc := range tests {
		got := decode(tc.input)
		if toString(got) != tc.want {
			t.Fatalf("expected: %v, got: %v", tc.want, got)
		}
	}

}

func bitsToInt(bits []int) int {
	val := 0
	for i := 0; i < len(bits); i++ {
		val <<= 1
		val += bits[i]
	}
	return val
}

func readInput() ([]byte, error) {
	input, err := os.ReadFile(FILE)
	if err != nil {
		log.Fatal(err)
	}

	return input, nil
}

type BitSlice struct {
	bits []int
}

func BitSliceFromHex(hex string) *BitSlice {
	return &BitSlice{decode(hex)}
}

func (bs *BitSlice) TakeInt(n int) int {
	val := bitsToInt(bs.bits[:n])
	bs.bits = bs.bits[n:]
	return val
}

func (bs *BitSlice) TakeBits(n int) []int {
	bits := bs.bits[:n]
	bs.bits = bs.bits[n:]
	return bits
}

func (bs *BitSlice) TakeLiteral() int {
	lit := []int{}
	keep := 1
	for keep == 1 {
		keep = bs.TakeInt(1)

		lit = append(lit, bs.TakeBits(4)...)
	}
	return bitsToInt(lit)
}

func (bs *BitSlice) Len() int {
	return len(bs.bits)
}

func (bs *BitSlice) ReadPackage() int {
	ver := bs.TakeInt(3)
	typ := bs.TakeInt(3)
	// log.Println("Ver:", ver)
	// log.Println("Typ:", typ)

	switch typ {
	case 4:
		_ = bs.TakeLiteral()
		// log.Println("Lit:", lit)
	default: // operator
		if bs.TakeInt(1) == 0 {
			len := bs.TakeInt(15)
			target := bs.Len() - len
			for bs.Len() > target {
				ver += bs.ReadPackage()
			}
		} else {
			n := bs.TakeInt(11)
			for i := 0; i < n; i++ {
				ver += bs.ReadPackage()
			}
		}
	}
	return ver
}

func TestPartOne(t *testing.T) {
	// input, err := readInput()
	// if err != nil {
	// 	t.Fatal(err)
	// }
	// _ = input
	data, err := os.ReadFile("input.txt")
	if err != nil {
		t.Fatal(err)
	}
	bs := BitSliceFromHex(string(data))
	ans := bs.ReadPackage()
	log.Println("Part 1:", ans)
}

func (bs *BitSlice) EvalBITS() int {
	_ = bs.TakeInt(3)
	typ := bs.TakeInt(3)

	if typ == 4 {
		return bs.TakeLiteral()
	}

	// Sub-pkgs
	args := []int{}
	if bs.TakeInt(1) == 0 {
		len := bs.TakeInt(15)
		target := bs.Len() - len
		for bs.Len() > target {
			args = append(args, bs.EvalBITS())
		}
	} else {
		n := bs.TakeInt(11)
		for i := 0; i < n; i++ {
			args = append(args, bs.EvalBITS())
		}
	}

	// Accumulate value of this pkg
	ans := 0
	switch typ {
	case 0: // sum
		for _, v := range args {
			ans += v
		}
	case 1: // product
		ans = 1
		for _, v := range args {
			ans *= v
		}
	case 2: // min
		ans = math.MaxInt
		for _, v := range args {
			if v < ans {
				ans = v
			}
		}
	case 3: // max
		ans = math.MinInt
		for _, v := range args {
			if v > ans {
				ans = v
			}
		}
	case 5: // greater than
		if args[0] > args[1] {
			ans = 1
		}
	case 6: // less than
		if args[0] < args[1] {
			ans = 1
		}
	case 7: // equal
		if args[0] == args[1] {
			ans = 1
		}
	default:
		panic("unknown operator")
	}
	return ans
}

func TestPartTwo(t *testing.T) {
	data, err := os.ReadFile("input.txt")
	if err != nil {
		t.Fatal(err)
	}
	bs := BitSliceFromHex(string(data))
	ans := bs.EvalBITS()
	log.Println("Part 2:", ans)
}

func TestPartTwoExamples(t *testing.T) {
	tests := []struct {
		input string
		want  int
	}{
		{"C200B40A82", 3},
		{"04005AC33890", 54},
		{"880086C3E88112", 7},
		{"CE00C43D881120", 9},
		{"D8005AC2A8F0", 1},
		{"F600BC2D8F", 0},
		{"9C005AC2F8F0", 0},
		{"9C0141080250320F1802104A08", 1},
	}

	for _, tt := range tests {
		bs := BitSliceFromHex(tt.input)
		got := bs.EvalBITS()
		if got != tt.want {
			t.Fatalf("expected: %v, got: %v", tt.want, got)
		}
	}
}
