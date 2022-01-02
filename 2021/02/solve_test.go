package solve

import (
	"bufio"
	"bytes"
	"errors"
	"log"
	"os"
	"strconv"
	"strings"
	"testing"
)

func MovePart1(input []byte) (int, int, error) {
	sc := bufio.NewScanner(bytes.NewBuffer(input))

	var depth, pos int
	for sc.Scan() {
		slice := strings.Split(sc.Text(), " ")

		dist, err := strconv.Atoi(slice[1])
		if err != nil {
			return 0, 0, err
		}
		switch slice[0] {
		case "up":
			depth -= dist
		case "down":
			depth += dist
		case "forward":
			pos += dist
		default:
			return 0, 0, errors.New("unknown direction: " + slice[0])
		}
	}
	return depth, pos, nil
}

func MovePart2(input []byte) (int, int, error) {
	sc := bufio.NewScanner(bytes.NewBuffer(input))

	var depth, pos, aim int
	for sc.Scan() {
		slice := strings.Split(sc.Text(), " ")

		dist, err := strconv.Atoi(slice[1])
		if err != nil {
			return 0, 0, err
		}
		switch slice[0] {
		case "up":
			aim -= dist
		case "down":
			aim += dist
		case "forward":
			pos += dist
			depth += aim * dist
		default:
			return 0, 0, errors.New("unknown direction: " + slice[0])
		}
	}
	return depth, pos, nil
}

func TestDay(t *testing.T) {

	input, err := os.ReadFile("input.txt")
	if err != nil {
		log.Fatal(err)
	}

	depth, pos, err := MovePart1(input)
	if err != nil {
		log.Fatal(err)
	}
	log.Println(depth * pos)

	depth, pos, err = MovePart2(input)
	if err != nil {
		log.Fatal(err)
	}
	log.Println(depth * pos)
}
