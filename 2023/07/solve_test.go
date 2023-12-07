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
	raw      []byte
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
	// Switch on how many we have of the most common card.
	slices.SortFunc(hand, func(a, b Card) int { return b.count - a.count })
	switch hand[0].count {
	case 5:
		return FiveKind
	case 4:
		return FourKind
	case 3:
		if hand[1].count == 2 {
			return FullHouse
		}
		return ThreeKind
	case 2:
		if hand[1].count == 2 {
			return TwoPair
		}
		return Pair
	case 1:
		return HighCard
	default:
		panic("bad hand")
	}
}

func Power(c byte) int {
	return strings.IndexByte("123456789TJQKA", c)
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
			raw:      []byte(cards),
		})
	}

	slices.SortFunc(hands, func(a, b Hand) int {
		d := a.strength - b.strength
		if d != 0 {
			return int(d)
		}
		// Tiebreaker
		return slices.CompareFunc(a.raw, b.raw, func(a, b byte) int {
			return Power(a) - Power(b)
		})
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
		return FiveKind
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
			raw:      []byte(cards),
		})
	}

	slices.SortFunc(hands, func(a, b Hand) int {
		d := a.strength - b.strength
		if d != 0 {
			return int(d)
		}
		// Tiebreaker.
		return slices.CompareFunc(a.raw, b.raw, func(a, b byte) int {
			return Power2(a) - Power2(b)
		})
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
