package solve

import (
	"log"
	"os"
	"testing"
)

const FILE = "input.txt"

func readInput() ([]byte, error) {
	input, err := os.ReadFile(FILE)
	if err != nil {
		log.Fatal(err)
	}

	return input, nil
}

type Dice struct {
	face  int
	rolls int
}

func (d *Dice) Roll() int {
	d.rolls++
	d.face++
	if d.face == 101 {
		d.face = 1
	}
	//log.Println("roll", d.face)
	return d.face
}

type Board struct {
	pos    int
	points int
}

func (b *Board) Move(d *Dice) {
	b.pos = Mod(b.pos+d.Roll()+d.Roll()+d.Roll(), 10)
	for b.pos > 10 {
		b.pos -= 10
	}
	b.points += b.pos
	//log.Println("move to", b.pos, "has points", b.points)
}

func TestPartOne(t *testing.T) {
	d := &Dice{}
	// Hard coded input.txt
	b1 := Board{5, 0}
	b2 := Board{10, 0}

	ans := 0
	for {
		b1.Move(d)
		if b1.points >= 1000 {
			ans = d.rolls * b2.points
			break
		}
		b2.Move(d)
		if b2.points >= 1000 {
			ans = d.rolls * b1.points
			break
		}
	}

	log.Println("Part 1:", ans)
}

func Mod(a, b int) int {
	for a > b {
		a -= b
	}
	return a
}

// Sum->Counts of sampling (1,2,3) three times w/ replacement.
var casts = map[int]int{3: 1, 4: 3, 5: 6, 6: 7, 7: 6, 8: 3, 9: 1}

type Wins struct {
	p1, p2 int
}

func Recur(w *Wins, player1, player2 Board, p1turn bool, uni int) {
	if p1turn {
		for steps, n := range casts {
			p1 := player1 // Copy for each iter
			p1.pos = Mod(p1.pos+steps, 10)
			p1.points += p1.pos

			if p1.points >= 21 {
				w.p1 += uni * n
			} else {
				Recur(w, p1, player2, false, uni*n)
			}
		}
	} else {
		for steps, n := range casts {
			p2 := player2 // Copy for each iter
			p2.pos = Mod(p2.pos+steps, 10)
			p2.points += p2.pos

			if p2.points >= 21 {
				w.p2 += uni * n
			} else {
				Recur(w, player1, p2, true, uni*n)
			}
		}
	}
}

func TestPartTwo(t *testing.T) {
	var w Wins
	Recur(&w, Board{5, 0}, Board{10, 0}, true, 1)
	log.Println("Part 2:", w)
}
