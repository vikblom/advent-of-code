package solve

import (
	"fmt"
	"slices"
	"strings"
	"testing"

	aoc "gitlab.com/vikblom/advent-of-code"

	_ "embed"
)

var (
	//go:embed "input.txt"
	input      string
	inputLines = strings.Split(strings.TrimRight(input, "\n"), "\n")
)

type Hand struct {
	strength Strength
	bid      int
	raw      string
}

type Card struct {
	face  rune
	count int
}

func (c Card) String() string {
	return fmt.Sprintf("%c:%d", c.face, c.count)
}

type Strength int

const (
	HighCard Strength = iota
	Pair
	TwoPair
	ThreeKind
	FullHouse
	FourKind
	FiveKind
)

func EstimateHand(hand []Card) Strength {
	// Switch on many _kinds_ of cards are in our hand.
	slices.SortFunc(hand, func(a, b Card) int { return b.count - a.count })
	switch len(hand) {
	case 1:
		return FiveKind
	case 2:
		if hand[0].count == 4 {
			return FourKind
		} else {
			return FullHouse
		}
	case 3:
		if hand[0].count == 3 {
			return ThreeKind
		} else {
			return TwoPair
		}
	case 4:
		return Pair
	case 5:
		return HighCard
	default:
		panic("bad hand")
	}
}

func Power(c byte) int {
	switch c {
	case 'A':
		return 14
	case 'K':
		return 13
	case 'Q':
		return 12
	case 'J':
		return 11
	case 'T':
		return 10
	default:
		return aoc.MustInt(string(c))
	}
}

func TestPartOne(t *testing.T) {
	hands := []Hand{}
	for _, s := range inputLines {
		cards, bid, _ := strings.Cut(s, " ")
		got := map[rune]int{}
		for _, c := range cards {
			got[c] += 1
		}

		hand := []Card{}
		for k, v := range got {
			hand = append(hand, Card{face: k, count: v})
		}

		hands = append(hands, Hand{
			strength: EstimateHand(hand),
			bid:      aoc.MustInt(bid),
			raw:      cards,
		})
	}

	slices.SortFunc(hands, func(a, b Hand) int {
		d := a.strength - b.strength
		if d != 0 {
			return int(d)
		}
		for i := 0; i < 5; i++ {
			a := Power(a.raw[i])
			b := Power(b.raw[i])
			if a != b {
				return a - b
			}
		}

		return 0
	})

	ans := 0
	rank := 0
	for _, h := range hands {
		// fmt.Printf("%v\n", h)
		rank += 1
		ans += rank * h.bid
	}

	aoc.Answer(t, ans, 256448566)
}

func EstimateHand2(hand []Card) Strength {
	// Switch on many _kinds_ of cards are in our hand.
	slices.SortFunc(hand, func(a, b Card) int { return b.count - a.count })

	// Do we even have jokers on hand?
	idx := slices.IndexFunc(hand, func(c Card) bool {
		return c.face == 'J'
	})
	if idx < 0 {
		return EstimateHand(hand)
	}

	// Slice out the jokers and count them towards the most common card.
	joker := hand[idx]
	hand = append(hand[:idx], hand[idx+1:]...)

	if len(hand) > 0 {
		hand[0].count += joker.count
	} else {
		// If we only had jokers, replace them with aces.
		hand = []Card{{face: 'A', count: joker.count}}
	}

	return EstimateHand(hand)
}

func Power2(c byte) int {
	if c == 'J' {
		return 0
	}
	return Power(c)
}

func TestPartTwo(t *testing.T) {
	hands := []Hand{}
	for _, s := range inputLines {
		cards, bid, _ := strings.Cut(s, " ")
		got := map[rune]int{}
		for _, c := range cards {
			got[c] += 1
		}

		hand := []Card{}
		for k, v := range got {
			hand = append(hand, Card{face: k, count: v})
		}

		hands = append(hands, Hand{
			strength: EstimateHand2(hand),
			bid:      aoc.MustInt(bid),
			raw:      cards,
		})
	}

	slices.SortFunc(hands, func(a, b Hand) int {
		d := a.strength - b.strength
		if d != 0 {
			return int(d)
		}
		for i := 0; i < 5; i++ {
			a := Power2(a.raw[i])
			b := Power2(b.raw[i])
			if a != b {
				return a - b
			}
		}

		return 0
	})

	ans := 0
	rank := 0
	for _, h := range hands {
		// fmt.Printf("%v\n", h)
		rank += 1
		ans += rank * h.bid
	}

	aoc.Answer(t, ans, 254412181)
}
