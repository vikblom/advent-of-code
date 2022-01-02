package solve

import (
	"bufio"
	"bytes"
	"fmt"
	"log"
	"os"
	"strings"
	"testing"
)

func FirstVisit(ss []string, target string) bool {
	for _, s := range ss {
		if s == target {
			return false
		}
	}
	return true
}

type Cave struct {
	key     string
	big     bool
	connect []*Cave
}

func NewCave(name string) *Cave {
	return &Cave{
		key:     name,
		big:     strings.ToUpper(name) == name,
		connect: []*Cave{},
	}
}

func (c *Cave) Pair(other *Cave) {
	c.connect = append(c.connect, other)
	other.connect = append(other.connect, c)
}

func (c *Cave) String() string {
	var s strings.Builder
	fmt.Fprintf(&s, "%s --", c.key)
	for _, cc := range c.connect {
		fmt.Fprintf(&s, " %s", cc.key)
	}
	return s.String()
}

type CaveSystem map[string]*Cave

func NewCaveSystem() CaveSystem {
	return make(map[string]*Cave)
}

// LookupCave or create if it does not exist.
func (cs CaveSystem) LookupCave(name string) *Cave {

	cave, ok := cs[name]
	if !ok {
		cave = NewCave(name)
	}
	return cave
}

func (cs CaveSystem) PathsToEnd(path []string) int {
	at := path[len(path)-1]
	if at == "end" {
		return 1 // this was 1 path
	}
	cave := cs.LookupCave(at)
	n := 0
	for _, c := range cave.connect {
		if c.big || FirstVisit(path, c.key) {
			n += cs.PathsToEnd(append(path, c.key))
		}
	}
	return n
}

func Part1(input []byte) (int, error) {
	sc := bufio.NewScanner(bytes.NewBuffer(input))

	cs := NewCaveSystem()

	for sc.Scan() {
		strs := strings.Split(sc.Text(), "-")

		cave1 := cs.LookupCave(strs[0])
		cs[cave1.key] = cave1
		cave2 := cs.LookupCave(strs[1])
		cs[cave2.key] = cave2

		cave1.Pair(cave2)
	}

	n := cs.PathsToEnd([]string{"start"})

	return n, nil
}

func (cs CaveSystem) PathsToEnd2(path []string, revisit bool) int {
	at := path[len(path)-1]
	if at == "end" {
		return 1 // this was 1 path
	}
	cave := cs.LookupCave(at)
	n := 0
	for _, c := range cave.connect {
		if c.big || FirstVisit(path, c.key) {
			n += cs.PathsToEnd2(append(path, c.key), revisit)
		} else if revisit && c.key != "start" {
			n += cs.PathsToEnd2(append(path, c.key), false)
		}
	}
	return n
}

func Part2(input []byte) (int, error) {
	sc := bufio.NewScanner(bytes.NewBuffer(input))

	cs := NewCaveSystem()

	for sc.Scan() {
		strs := strings.Split(sc.Text(), "-")

		cave1 := cs.LookupCave(strs[0])
		cs[cave1.key] = cave1
		cave2 := cs.LookupCave(strs[1])
		cs[cave2.key] = cave2

		cave1.Pair(cave2)
	}

	n := cs.PathsToEnd2([]string{"start"}, true)

	return n, nil
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
