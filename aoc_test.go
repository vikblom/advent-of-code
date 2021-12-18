package aoc_test

import (
	"testing"

	aoc "gitlab.com/vikblom/advent-of-code"
)

func TestIntSqrt(t *testing.T) {

	tests := []struct {
		input int
		want  int
	}{
		{0, 0},
		{1, 1},
		{4, 2},
		{25, 5},
		{26, 5}, // Rounds down
		{29, 5}, // Rounds down
	}

	for _, tc := range tests {
		got := aoc.IntSqrt(tc.input)
		if got != tc.want {
			t.Fatalf("expected: %v, got: %v", tc.want, got)
		}
	}

}
