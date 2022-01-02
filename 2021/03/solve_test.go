package solve

import (
	"bufio"
	"bytes"
	"log"
	"os"
	"strconv"
	"sync"
	"testing"
)

func Part1(input []byte) (int, error) {
	sc := bufio.NewScanner(bytes.NewBuffer(input))

	var once sync.Once
	var counts []int
	rows := 0
	for sc.Scan() {
		rows += 1

		num := sc.Text()
		once.Do(func() {
			counts = make([]int, len(num))
		})

		for i, d := range num {
			if d == '1' {
				counts[i] += 1
			}
		}
	}

	var gamma int
	for i, v := range counts {
		if v > rows/2 {
			gamma = gamma | (1 << (len(counts) - i - 1))
		}
	}
	epsilon := (^gamma) & (1<<len(counts) - 1)

	return gamma * epsilon, nil
}

func Split(in []int, digit int) (zero []int, one []int) {
	//log.Println("Split")
	for _, v := range in {
		//log.Printf("    %.6b\n", v)
		if (v & (1 << (digit - 1))) == 0 {
			zero = append(zero, v)
		} else {
			one = append(one, v)
		}
	}
	return
}

func Oxygen(nums []int, digit int) int {
	if len(nums) == 1 {
		return nums[0]
	}
	zeros, ones := Split(nums, digit)
	if len(ones) >= len(zeros) {
		return Oxygen(ones, digit-1)
	} else {
		return Oxygen(zeros, digit-1)
	}
}

func C02(nums []int, digit int) int {
	if len(nums) == 1 {
		return nums[0]
	}
	zeros, ones := Split(nums, digit)
	if len(zeros) <= len(ones) {
		return C02(zeros, digit-1)
	} else {
		return C02(ones, digit-1)
	}
}

func Part2(input []byte) (int, error) {
	sc := bufio.NewScanner(bytes.NewBuffer(input))

	nums := []int{}
	digits := 0
	for sc.Scan() {
		if digits == 0 {
			digits = len(sc.Text())
		}
		i, err := strconv.ParseInt(sc.Text(), 2, 64)
		if err != nil {
			return 0, err
		}
		nums = append(nums, int(i))
	}

	return Oxygen(nums, digits) * C02(nums, digits), nil
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
