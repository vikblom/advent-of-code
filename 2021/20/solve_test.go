package solve

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"math"
	"os"
	"strings"
	"testing"
)

type Px struct {
	y int // row
	x int // col
}

type Image map[Px]bool

const FILE = "input.txt"

func readInput() (Image, []byte, error) {
	input, err := os.ReadFile(FILE)
	if err != nil {
		log.Fatal(err)
	}

	buf := bytes.NewBuffer(input)
	enh, err := buf.ReadBytes('\n')
	if err != nil {
		return nil, nil, err
	}
	enh = enh[:len(enh)-1]

	tmp, err := buf.ReadByte()
	if err != nil {
		return nil, nil, err
	}
	if tmp != '\n' {
		return nil, nil, fmt.Errorf("expected newline, got %c", tmp)
	}

	image := make(Image)
	row := 0
	col := 0
	for {
		b, err := buf.ReadByte()
		if err == io.EOF {
			break
		} else if err != nil {
			return nil, nil, err
		}
		if b == '\n' {
			col = 0
			row++
		} else {
			image[Px{row, col}] = b == '#'
			col++
		}
	}

	return image, enh, nil
}

func (img Image) Limits() (int, int, int, int) {

	xmin := math.MaxInt
	xmax := math.MinInt
	ymin := math.MaxInt
	ymax := math.MinInt

	for p := range img {
		if p.x > xmax {
			xmax = p.x
		}
		if p.x < xmin {
			xmin = p.x
		}
		if p.y > ymax {
			ymax = p.y
		}
		if p.y < ymin {
			ymin = p.y
		}
	}
	return xmin, xmax, ymin, ymax
}

func (img Image) String() string {
	xmin, xmax, ymin, ymax := img.Limits()

	var s strings.Builder
	for y := ymin; y < ymax+1; y++ {
		for x := xmin; x < xmax+1; x++ {
			c, ok := img[Px{y, x}]
			if ok && c {
				fmt.Fprintf(&s, "#")
			} else {
				fmt.Fprintf(&s, ".")
			}
		}
		fmt.Fprintf(&s, "\n")
	}
	return s.String()
}

func (img Image) EnhancePixel(row, col int, backlit bool) int {
	i := 0
	for r := row - 1; r <= row+1; r++ {
		for c := col - 1; c <= col+1; c++ {
			i <<= 1
			val, ok := img[Px{r, c}]
			if (ok && val) || (!ok && backlit) {
				i |= 0x1
			}
		}
	}
	return i
}

func (img Image) Enhance(enh []byte, backlit bool) (Image, bool) {

	enhanced := make(Image)
	xmin, xmax, ymin, ymax := img.Limits()
	for y := ymin - 1; y < ymax+2; y++ {
		for x := xmin - 1; x < xmax+2; x++ {
			if _, ok := enhanced[Px{y, x}]; ok {
				continue // already calculated
			}
			enhanced[Px{y, x}] = '#' == enh[img.EnhancePixel(y, x, backlit)]
		}
	}
	if backlit {
		backlit = enh[len(enh)-1] == '#'
	} else {
		backlit = enh[0] == '#'
	}

	return enhanced, backlit
}

func TestPartOne(t *testing.T) {
	img, enh, err := readInput()
	if err != nil {
		t.Fatal(err)
	}

	backlit := false
	for i := 0; i < 2; i++ {
		img, backlit = img.Enhance(enh, backlit)
	}

	count := 0
	for _, v := range img {
		if v {
			count++
		}
	}

	log.Println("Part 1:", count)
}

func TestPartTwo(t *testing.T) {
	img, enh, err := readInput()
	if err != nil {
		t.Fatal(err)
	}

	backlit := false
	for i := 0; i < 50; i++ {
		img, backlit = img.Enhance(enh, backlit)
	}

	count := 0
	for _, v := range img {
		if v {
			count++
		}
	}

	log.Println("Part 2:", count)
}
