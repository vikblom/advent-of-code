package solve

import (
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

type signal int

const (
	low signal = iota
	high
	noop
)

func (s signal) String() string {
	if s == low {
		return "low"
	}
	return "high"
}

// % - flip flop, swaps state on low pulse.
// & - conjunction, remember inputs, iff all memory low sends high.
// broadcast - multiplex
// button - sends a single low pulse to broadcaster.

type Module struct {
	kind    byte
	name    string
	targets []string

	// Only for flip
	state bool

	// Only for conj
	memory map[string]signal
}

func (m *Module) Handle(p Pulse) signal {
	switch m.kind {
	case '%':
		if p.s == high {
			return noop
		}
		if p.s == low {
			m.state = !m.state
		}
		if m.state {
			return high
		}
		return low

	case '&':
		m.memory[p.from] = p.s
		for _, v := range m.memory {
			if v == low {
				return high
			}
		}
		return low

	default:
		panic("bad input")
	}
}

type Pulse struct {
	to, from string
	s        signal
}

func TestPartOne(t *testing.T) {
	broadcast := []string{}
	// k has been sent to from v.
	receivers := map[string][]string{}

	modules := map[string]*Module{}
	for _, l := range inputLines {
		if strings.HasPrefix(l, "broadcaster") {
			_, rest, _ := strings.Cut(l, " -> ")
			for _, r := range strings.Split(rest, ", ") {
				broadcast = append(broadcast, r)
			}
			continue
		}
		kind := l[0]
		name, tail, _ := strings.Cut(l[1:], " -> ")
		targets := strings.Split(tail, ", ")

		for _, t := range targets {
			receivers[t] = append(receivers[t], name)
		}

		modules[name] = &Module{
			kind:    kind,
			name:    name,
			targets: targets,
		}
	}

	// Populate conjunction modules with their senders.
	for _, v := range modules {
		if v.kind != '&' {
			continue
		}
		v.memory = map[string]signal{}
		for _, r := range receivers[v.name] {
			v.memory[r] = low
		}
	}

	var lows, highs = 0, 0
	for i := 0; i < 1000; i++ {
		// Button push
		lows += 1

		pulses := []Pulse{}
		for _, t := range broadcast {
			pulses = append(pulses, Pulse{
				from: "broadcast",
				to:   t,
				s:    low,
			})
		}

		for len(pulses) > 0 {
			p := pulses[0]
			pulses = pulses[1:]

			if p.s == low {
				lows += 1
			} else {
				highs += 1
			}

			recv, ok := modules[p.to]
			if !ok {
				continue
			}
			next := recv.Handle(p)
			if next == noop {
				continue
			}
			for _, t := range recv.targets {
				pulses = append(pulses, Pulse{
					from: recv.name,
					to:   t,
					s:    next,
				})
			}
		}
	}

	aoc.Answer(t, lows*highs, 788081152)
}

func TestPartTwo(t *testing.T) {
	broadcast := []string{}
	// k has been sent to from v.
	receivers := map[string][]string{}

	modules := map[string]*Module{}
	for _, l := range inputLines {
		if strings.HasPrefix(l, "broadcaster") {
			_, rest, _ := strings.Cut(l, " -> ")
			for _, r := range strings.Split(rest, ", ") {
				broadcast = append(broadcast, r)
			}
			continue
		}
		kind := l[0]
		name, tail, _ := strings.Cut(l[1:], " -> ")
		targets := strings.Split(tail, ", ")

		for _, t := range targets {
			receivers[t] = append(receivers[t], name)
		}

		modules[name] = &Module{
			kind:    kind,
			name:    name,
			targets: targets,
		}
	}

	// Populate conjunction modules with their senders.
	for _, v := range modules {
		if v.kind != '&' {
			continue
		}
		v.memory = map[string]signal{}
		for _, r := range receivers[v.name] {
			v.memory[r] = low
		}
	}

	feeder := receivers["rx"][0]
	cycles := map[string]int{}

	buttons := 0
Done:
	for {
		buttons += 1

		pulses := []Pulse{}
		for _, t := range broadcast {
			pulses = append(pulses, Pulse{
				from: "broadcast",
				to:   t,
				s:    low,
			})
		}

		for len(pulses) > 0 {
			p := pulses[0]
			pulses = pulses[1:]

			recv, ok := modules[p.to]
			if !ok {
				continue
			}

			if recv.name == feeder {
				for _, into := range receivers[feeder] {
					if recv.memory[into] == high {
						if _, ok := cycles[into]; !ok {
							cycles[into] = buttons
						}
					}
					if len(cycles) == len(receivers[feeder]) {
						break Done
					}
				}
			}

			next := recv.Handle(p)
			if next == noop {
				continue
			}
			for _, t := range recv.targets {
				pulses = append(pulses, Pulse{
					from: recv.name,
					to:   t,
					s:    next,
				})
			}
		}
	}

	vs := []int{}
	for _, v := range cycles {
		vs = append(vs, v)
	}
	ans := aoc.LCM(vs[0], vs[1:]...)

	aoc.Answer(t, ans, 224602011344203)
}
