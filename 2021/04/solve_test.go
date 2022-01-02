package solve

import (
	"bufio"
	"bytes"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
	"testing"
)

func ScanCSV(data []byte, atEOF bool) (advance int, token []byte, err error) {
	if atEOF && len(data) == 0 {
		return 0, nil, nil
	}
	if i := bytes.IndexByte(data, ','); i >= 0 {
		// We have value with comma after
		return i + 1, data[0:i], nil
	}
	// No more data, no more comma, the remainder is the final value
	if atEOF {
		return len(data), data, nil
	}
	return 0, nil, nil
}

func ReadCSV(data []byte) ([]int, error) {
	rdr := bytes.NewBuffer(data)
	sc := bufio.NewScanner(rdr)
	sc.Split(ScanCSV)

	draws := []int{}
	for sc.Scan() {
		v, err := strconv.ParseInt(sc.Text(), 10, 64)
		if err != nil {
			return nil, err
		}
		draws = append(draws, int(v))
	}
	return draws, nil
}

type Slot struct {
	Number int
	Marked bool
}

type Board [5][5]Slot

func ReadInput(input []byte) ([]int, []*Board, error) {
	sc := bufio.NewScanner(bytes.NewBuffer(input))
	sc.Split(bufio.ScanWords)

	if !sc.Scan() {
		return nil, nil, fmt.Errorf("could not scan input: %w", sc.Err())
	}
	//log.Printf("%v\n", sc.Text())

	draws, err := ReadCSV(sc.Bytes())
	if err != nil {
		return nil, nil, err
	}
	//log.Printf("%#v\n", draws)

	boards := []*Board{}
fill:
	for {
		b := Board{}
		for row := 0; row < 5; row++ {
			for col := 0; col < 5; col++ {
				ok := sc.Scan()
				if !ok {
					break fill
				}
				v, err := strconv.ParseInt(sc.Text(), 10, 64)
				if err != nil {
					return nil, nil, err
				}
				b[row][col] = Slot{Number: int(v)}
			}
		}
		boards = append(boards, &b)
	}

	return draws, boards, nil
}

func (b *Board) String() string {
	var s strings.Builder
	for row := 0; row < 5; row++ {
		for col := 0; col < 5; col++ {
			if b[row][col].Marked {
				fmt.Fprintf(&s, "  X ")

			} else {
				fmt.Fprintf(&s, "%3d ", b[row][col].Number)
			}
		}
		fmt.Fprintf(&s, "\n")
	}
	return s.String()
}

func (b *Board) SumUnmarked() int {
	sum := 0
	for row := 0; row < 5; row++ {
		for col := 0; col < 5; col++ {
			if !b[row][col].Marked {
				sum += b[row][col].Number
			}
		}
	}
	return sum
}

// Check if mark added on row,col made this board a winnder.
// If so, return the answer metric, else return -1.
func (b *Board) Check(row, col int) int {
	inRow := 0
	inCol := 0
	for i := 0; i < 5; i++ {
		if b[row][i].Marked {
			inRow++
		}
		if b[i][col].Marked {
			inCol++
		}
	}
	if inRow == 5 || inCol == 5 {
		return b.SumUnmarked()
	}
	return -1
}

func (b *Board) Draw(d int) int {
	for row := 0; row < 5; row++ {
		for col := 0; col < 5; col++ {
			if b[row][col].Number == d {
				b[row][col].Marked = true
				if c := b.Check(row, col); c >= 0 {
					return d * c
				}
			}
		}
	}
	return -1
}

func Part1(input []byte) (int, error) {
	draws, boards, err := ReadInput(input)
	if err != nil {
		return 0, err
	}

	for _, d := range draws {
		for _, b := range boards {
			if c := b.Draw(d); c >= 0 {
				return c, nil
			}
		}
	}
	println("NO WINNER")
	return 0, nil
}

func Part2(input []byte) (int, error) {
	draws, boards, err := ReadInput(input)
	if err != nil {
		return 0, err
	}

	var worstDraws, worstScore int
	for _, b := range boards {
		for i, d := range draws {
			if c := b.Draw(d); c >= 0 {
				if i > worstDraws {
					worstDraws = i
					worstScore = c
				}
				break
			}
		}
	}
	return worstScore, nil
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
