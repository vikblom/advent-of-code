package solve

import (
	"maps"
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

type Rule struct {
	// If no label, always target.
	label  string
	op     byte
	val    int
	target string
}

func TestPartOne(t *testing.T) {
	upper, lower, _ := strings.Cut(input, "\n\n")

	rules := map[string][]Rule{}
	for _, l := range strings.Split(strings.TrimRight(upper, "\n"), "\n") {
		name, rest, _ := strings.Cut(l[:len(l)-1], "{")

		rs := []Rule{}
		for _, v := range strings.Split(rest, ",") {
			check, tar, ok := strings.Cut(v, ":")
			if !ok {
				rs = append(rs, Rule{target: v})
				break
			}
			i := strings.IndexAny(check, "<=>")
			if i < 0 {
				panic("bad input")
			}
			rs = append(rs, Rule{
				label:  check[:i],
				op:     check[i],
				val:    aoc.MustInt(check[i+1:]),
				target: tar,
			})
		}
		rules[name] = rs
	}

	parts := []map[string]int{}
	for _, l := range strings.Split(strings.TrimRight(lower, "\n"), "\n") {
		part := map[string]int{}
		for _, v := range strings.Split(l[1:len(l)-1], ",") {
			label, val, _ := strings.Cut(v, "=")
			part[label] = aoc.MustInt(val)
		}
		parts = append(parts, part)
	}

	ans := 0
	for _, p := range parts {
		at := "in"
		for !(at == "A" || at == "R") {
			for _, rule := range rules[at] {
				if rule.label == "" {
					at = rule.target
					break
				}

				var match bool
				have := p[rule.label]
				switch rule.op {
				case '<':
					match = have < rule.val
				case '>':
					match = have > rule.val
				}
				if match {
					at = rule.target
					break
				}
			}
		}

		if at == "A" {
			for _, v := range p {
				ans += v
			}
		}
	}
	aoc.Answer(t, ans, 383682)
}

type Interval struct {
	start, end int
}

func (i *Interval) Empty() bool {
	return i.start == i.end
}

func (i *Interval) Length() int {
	return i.end - i.start
}

type Part struct {
	at string
	v  map[string]Interval
}

func (p *Part) Empty() bool {
	for _, v := range p.v {
		if !v.Empty() {
			return false
		}
	}
	return true
}

func TestPartTwo(t *testing.T) {
	upper, _, _ := strings.Cut(input, "\n\n")

	rules := map[string][]Rule{}
	for _, l := range strings.Split(strings.TrimRight(upper, "\n"), "\n") {
		name, rest, _ := strings.Cut(l[:len(l)-1], "{")

		rs := []Rule{}
		for _, v := range strings.Split(rest, ",") {
			check, tar, ok := strings.Cut(v, ":")
			if !ok {
				rs = append(rs, Rule{target: v})
				break
			}
			i := strings.IndexAny(check, "<=>")
			if i < 0 {
				panic("bad input")
			}
			rs = append(rs, Rule{
				label:  check[:i],
				op:     check[i],
				val:    aoc.MustInt(check[i+1:]),
				target: tar,
			})
		}
		rules[name] = rs
	}

	parts := []Part{{
		at: "in",
		v: map[string]Interval{
			"x": {1, 4001},
			"m": {1, 4001},
			"a": {1, 4001},
			"s": {1, 4001},
		},
	}}
	done := []Part{}

	for len(parts) > 0 {
		p := parts[0]
		parts = parts[1:]

		if p.at == "A" {
			done = append(done, p)
			continue
		}
		if p.at == "R" {
			continue
		}

		for _, rule := range rules[p.at] {
			if rule.label == "" {
				parts = append(parts, Part{
					at: rule.target,
					v:  maps.Clone(p.v),
				})
				break
			}

			// Overlap
			// interval [start, end)
			// rule     target?val
			//
			// If we match we need to divvy up [start, target) and [target, end).
			interval := p.v[rule.label]
			if rule.val < interval.start || interval.end <= rule.val {
				// Outside, doesn't affect us.
				continue
			}

			// If there is a match, the covered area should go to target.
			// And p should shrink.
			vv := maps.Clone(p.v)
			switch rule.op {
			case '<':
				vv[rule.label] = Interval{
					start: interval.start,
					end:   rule.val,
				}
				p.v[rule.label] = Interval{
					start: rule.val,
					end:   interval.end,
				}
			case '>':
				vv[rule.label] = Interval{
					start: rule.val + 1,
					end:   interval.end,
				}
				p.v[rule.label] = Interval{
					start: interval.start,
					end:   rule.val + 1,
				}
			default:
				panic("bad input")
			}
			parts = append(parts, Part{
				at: rule.target,
				v:  vv,
			})
			// Keep slicing p.
		}
	}

	ans := 0
	for _, d := range done {
		n := 1
		for _, v := range d.v {
			n *= v.Length()
		}
		ans += n
	}
	aoc.Answer(t, ans, 117954800808317)
}
