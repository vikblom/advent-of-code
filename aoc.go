package aoc

import (
	"bufio"
	"io"
	"strconv"
)

func ReadInts(rdr io.Reader) ([]int, error) {
	sc := bufio.NewScanner(rdr)
	data := []int{}
	for sc.Scan() {
		x, err := strconv.Atoi(sc.Text())
		if err != nil {
			return nil, err
		}
		data = append(data, x)
	}
	return data, nil
}
