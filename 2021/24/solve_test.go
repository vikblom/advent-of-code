package solve

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"log"
	"os"
	"strconv"
	"strings"
	"testing"
)

type Op int

const (
	OP_INPUT Op = iota
	OP_ADD
	OP_MUL
	OP_DIV
	OP_MOD
	OP_EQL
)

var opcodes = map[string]Op{
	"inp": OP_INPUT,
	"add": OP_ADD,
	"mul": OP_MUL,
	"div": OP_DIV,
	"mod": OP_MOD,
	"eql": OP_EQL,
}

type Instr struct {
	op   Op
	a    int
	bchr int // variable b
	bnum int // literal b
}

func (i Instr) String() string {
	for name, v := range opcodes {
		if v == i.op {
			if i.bchr > 0 {
				return fmt.Sprintf("%s %c %c", name, i.a, i.bchr)
			} else {
				return fmt.Sprintf("%s %c %d", name, i.a, i.bnum)
			}

		}
	}
	return ""
}

type Program []Instr

const FILE = "input.txt"

func Parse(program io.Reader) (Program, error) {
	sc := bufio.NewScanner(program)

	prog := Program{}
	for sc.Scan() {
		parts := strings.Split(strings.TrimSpace(sc.Text()), " ")
		op := opcodes[parts[0]]
		a := int(parts[1][0])
		i := Instr{
			op: op,
			a:  a,
		}
		if op != OP_INPUT {
			c := parts[2][0]
			if 'w' <= c && c <= 'z' {
				i.bchr = int(c)
			} else {
				num, err := strconv.Atoi(parts[2])
				if err != nil {
					return nil, err
				}
				i.bnum = num
			}
		}
		prog = append(prog, i)

	}

	return prog, nil
}

func ReadSteps(prog Program) []Step {
	splits := []Program{}
	next := Program{}
	for _, i := range prog {
		if i.op == OP_INPUT && len(next) != 0 {
			splits = append(splits, next)
			next = Program{i}
		} else {
			next = append(next, i)
		}
	}
	splits = append(splits, next)

	steps := []Step{}
	for _, s := range splits {
		steps = append(steps, Step{
			D: s[4].bnum,
			A: s[5].bnum,
			B: s[15].bnum,
		})
	}
	return steps
}

type Pair struct {
	idx int
	add int
}

type Step struct {
	D int
	A int
	B int
}

func FindModelNo(steps []Step, inputs []int) int {
	stack := []Pair{}
	for i, s := range steps {

		// The program is the same subprogram 14 times.
		// The subprogram works like a stack that push/pops.
		// Push always happens when D==1.
		// Pop happens if D!=1 and input matches a condition.
		// If we cannot pick an input satisfying the cond, we might
		// have to tweak the input of the push that added the value.
		// Since that value was a push, it will only ever be subject to
		// one and only one such condition.

		if s.D == 1 {
			// Remember which input this came from.
			// Might be adjusted later to match a cond.
			stack = append(stack, Pair{i, s.B})
		} else {
			p := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			inputs[i] = inputs[p.idx] + p.add + s.A
			// If out bounds, both sides must be adjusted the same amount.
			if inputs[i] > 9 {
				// We guessed too high, i.e. part 1, stay high.
				d := inputs[i] - 9
				inputs[i] -= d
				inputs[p.idx] -= d
			} else if inputs[i] < 1 {
				// We guessed too low, i.e. part 2, stay high.
				d := inputs[i] - 1
				inputs[i] -= d
				inputs[p.idx] -= d
			}
		}
	}
	ans := 0
	pow := 1
	for i := len(inputs) - 1; i >= 0; i-- {
		ans += pow * inputs[i]
		pow *= 10
	}
	return ans
}

func TestPartOne(t *testing.T) {
	input, err := os.ReadFile(FILE)
	if err != nil {
		log.Fatal(err)
	}
	prog, err := Parse(bytes.NewReader(input))
	if err != nil {
		log.Fatal(err)
	}

	steps := ReadSteps(prog)

	// Start from biggest possible input.
	inputs := make([]int, 14)
	for i := range inputs {
		inputs[i] = 9
	}

	log.Println("Part 1:", FindModelNo(steps, inputs))
}

func TestPartTwo(t *testing.T) {
	input, err := os.ReadFile(FILE)
	if err != nil {
		log.Fatal(err)
	}
	prog, err := Parse(bytes.NewReader(input))
	if err != nil {
		log.Fatal(err)
	}

	steps := ReadSteps(prog)

	// Start from smallest possible input.
	inputs := make([]int, 14)
	for i := range inputs {
		inputs[i] = 1
	}

	log.Println("Part 2:", FindModelNo(steps, inputs))
}
