package solve

import (
	"fmt"
	"strings"
	"testing"

	"github.com/aclements/go-z3/z3"
	aoc "gitlab.com/vikblom/advent-of-code"

	_ "embed"
)

// Example: two paths cross in [7,27].

var (
	//go:embed "input.txt"
	input      string
	inputLines = strings.Split(strings.TrimRight(input, "\n"), "\n")
)

type pt3 struct {
	x, y, z float64
}

type hail struct {
	p  pt3
	dp pt3
}

// how do we start at [19,13]
// A1 + DX1*x = A2 + DX2*x
//
// x*(DX1 - DX2) = A2 - A1
//
// t1 = (A2-A1)/(DX1-DX2)
//
// B1 + DY1*y = B2 + DY2*x
//
// t2 = (B2 - B1) / (DY1 - DY2)
//
// Don't care if t1 != t2.

// Express each Y in terms of X: ax+c and bx+d
// x = A1 + DX1 * t
// t = (x-A1) / DX1
//
// y1 = B1 + DY1 * (x1-A1) / DX1
//    = B1 - DY1*A1/DX1 + x1*DY1/DX1
// so
//   a = DY1/DX1
//   c = B1 - A1 * DY1/DX1
//
// y2 = B2 + DY2 & (x2-A2) / DX2
// similarly
//   b = DY2/DX2
//   d = B2 - A2 * DY2/DX2

func TestPartOne(t *testing.T) {
	hails := []hail{}
	for _, l := range inputLines {
		l = strings.ReplaceAll(l, " @ ", ", ")
		l = strings.ReplaceAll(l, ",", " ")
		ls := strings.Fields(l)
		hails = append(hails, hail{
			p: pt3{
				x: aoc.MustFloat(ls[0]),
				y: aoc.MustFloat(ls[1]),
				z: aoc.MustFloat(ls[2]),
			},
			dp: pt3{
				x: aoc.MustFloat(ls[3]),
				y: aoc.MustFloat(ls[4]),
				z: aoc.MustFloat(ls[5]),
			},
		})
	}

	min := 200000000000000.0
	max := 400000000000000.0
	ans := 0
	for i, one := range hails[:len(hails)-1] {
		for _, two := range hails[i+1:] {
			// if one.dp.x == two.dp.x || one.dp.y == two.dp.y {
			// 	fmt.Println("zero")
			// 	continue
			// }

			a := one.dp.y / one.dp.x
			c := one.p.y - one.p.x*a

			b := two.dp.y / two.dp.x
			d := two.p.y - two.p.x*b

			if a == b {
				continue
			}

			x := (d - c) / (a - b)
			y := a*x + c

			// x = x0 + dx*t
			// t = (x-x0) / dx

			// Must check that this is in the future.
			t1 := (x - one.p.x) / one.dp.x
			t2 := (x - two.p.x) / two.dp.x

			future := 0 <= t1 && 0 <= t2
			inbounds := min <= x && x <= max && min <= y && y <= max
			if inbounds && future {
				// fmt.Println(one, "vs", two)
				// fmt.Println(x, y)
				ans += 1
			}
		}
	}

	aoc.Answer(t, ans)
}

type xyz struct {
	x, y, z int
}

type hail2 struct {
	p  xyz
	dp xyz
}

// t0, t1, t2
//
// we're looking for x,y,z and dx,dy,dz
// such that
//
// x + t0*dx = ax + t0*adx // line a
// y + t0*dy = ay + t0*ady
// z + t0*dz = az + t0*adz
//
// x + t1*dx = bx + t1*bdx // line b
// y + t1*dy = by + t1*bdy
// z + t1*dz = bz + t1*bdz
//
// x + t2*dx = cx + t2*cdx // line c
// y + t2*dy = cy + t2*cdy
// z + t2*dz = cz + t2*cdz
//

func TestPartTwo(t *testing.T) {
	hails := []hail2{}
	for _, l := range inputLines {
		l = strings.ReplaceAll(l, " @ ", ", ")
		l = strings.ReplaceAll(l, ",", " ")
		ls := strings.Fields(l)
		hails = append(hails, hail2{
			p: xyz{
				x: aoc.MustInt(ls[0]),
				y: aoc.MustInt(ls[1]),
				z: aoc.MustInt(ls[2]),
			},
			dp: xyz{
				x: aoc.MustInt(ls[3]),
				y: aoc.MustInt(ls[4]),
				z: aoc.MustInt(ls[5]),
			},
		})
	}
	// for _, h := range hails {
	// 	fmt.Println(h)
	// }
	c := z3.NewContext(z3.NewContextConfig())
	s := z3.NewSolver(c)

	x := c.IntConst("x")

	s.Assert(x.Eq(c.FromInt(4, c.IntSort()).(z3.Int)))

	sat, err := s.Check()
	if err != nil {
		t.Fatalf("check: %s", err)
	}
	if !sat {
		t.Fatalf("not satisfied")
	}
	v := s.Model().Eval(x, false)
	fmt.Println(v.String())

	ans := 0
	aoc.Answer(t, ans)
}
