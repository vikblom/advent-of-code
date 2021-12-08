package aoc

import (
	"bufio"
	"bytes"
	"io"
	"os"
	"strconv"
	"strings"
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

func ReadCSV(filename string) ([]int, error) {
	content, err := os.ReadFile(filename)
	if err != nil {
		return nil, err
	}

	r := bytes.NewBuffer(content)
	sc := bufio.NewScanner(r)
	sc.Split(ScanCSV)

	ints := []int{}
	for sc.Scan() {
		f, err := strconv.Atoi(strings.TrimSpace(sc.Text()))
		if err != nil {
			return nil, err
		}
		ints = append(ints, f)
	}
	return ints, nil
}

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
