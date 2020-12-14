package main

import (
	"fmt"

	"github.com/fernomac/advent2020/lib"
)

func main() {
	g := load()
	fmt.Println(partOne(g))
	fmt.Println(partTwo(g))
}

func partOne(g grid) int {
	for {
		var c int
		g, c = g.step()
		// g.print()
		if c == 0 {
			return g.count()
		}
	}
}

func partTwo(g grid) int {
	for {
		var c int
		g, c = g.stepLOS()
		// g.print()
		if c == 0 {
			return g.count()
		}
	}
}

type state int

const (
	floor state = iota
	empty
	full
)

type grid struct {
	arr    [][]state
	lx, ly int
}

func (g *grid) step() (grid, int) {
	newarr := make([][]state, len(g.arr))
	changes := 0

	for y := 0; y < len(g.arr); y++ {
		row := g.arr[y]
		newrow := make([]state, len(row))

		for x := 0; x < len(row); x++ {
			newstate, diff := g.next(x, y)
			newrow[x] = newstate
			if diff {
				changes++
			}
		}

		newarr[y] = newrow
	}

	return grid{newarr, g.lx, g.ly}, changes
}

func (g *grid) stepLOS() (grid, int) {
	newarr := make([][]state, len(g.arr))
	changes := 0

	for y := 0; y < len(g.arr); y++ {
		row := g.arr[y]
		newrow := make([]state, len(row))

		for x := 0; x < len(row); x++ {
			newstate, diff := g.nextLOS(x, y)
			newrow[x] = newstate
			if diff {
				changes++
			}
		}

		newarr[y] = newrow
	}

	return grid{newarr, g.lx, g.ly}, changes
}

func (g *grid) next(x, y int) (state, bool) {
	switch g.arr[y][x] {
	case floor:
		return floor, false

	case empty:
		if g.neighbors(x, y) == 0 {
			return full, true
		}
		return empty, false

	case full:
		if g.neighbors(x, y) >= 4 {
			return empty, true
		}
		return full, false

	default:
		panic("bad state")
	}
}

func (g *grid) nextLOS(x, y int) (state, bool) {
	switch g.arr[y][x] {
	case floor:
		return floor, false

	case empty:
		if g.neighborsLOS(x, y) == 0 {
			return full, true
		}
		return empty, false

	case full:
		if g.neighborsLOS(x, y) >= 5 {
			return empty, true
		}
		return full, false

	default:
		panic("bad state")
	}
}

func (g *grid) neighbors(x, y int) int {
	count := 0

	for dy := -1; dy <= 1; dy++ {
		for dx := -1; dx <= 1; dx++ {
			if dx != 0 || dy != 0 {
				s := g.at(x+dx, y+dy)
				if s == full {
					count++
				}
			}
		}
	}

	return count
}

func (g *grid) neighborsLOS(x, y int) int {
	count := 0

	for dy := -1; dy <= 1; dy++ {
		for dx := -1; dx <= 1; dx++ {
			if dx != 0 || dy != 0 {
				s := g.lineOfSight(x, y, dx, dy)
				if s == full {
					count++
				}
			}
		}
	}

	return count
}

func (g *grid) lineOfSight(x, y int, dx, dy int) state {
	for {
		x += dx
		y += dy
		s := g.at(x, y)
		if s != floor {
			return s
		}
	}
}

func (g *grid) at(x, y int) state {
	if y < 0 || y >= g.ly {
		return empty
	}
	if x < 0 || x >= g.lx {
		return empty
	}

	return g.arr[y][x]
}

func (g *grid) count() int {
	count := 0
	for y := 0; y < g.ly; y++ {
		for x := 0; x < g.lx; x++ {
			if g.arr[y][x] == full {
				count++
			}
		}
	}
	return count
}

func (g *grid) print() {
	for y := 0; y < g.ly; y++ {
		for x := 0; x < g.lx; x++ {
			switch g.arr[y][x] {
			case floor:
				fmt.Print(".")
			case empty:
				fmt.Print("L")
			case full:
				fmt.Print("#")
			default:
				panic("bad state")
			}
		}
		fmt.Println()
	}
	fmt.Println()
}

func load() grid {
	r := lib.ReadLines("input.txt")
	defer r.Close()

	arr := [][]state{}

	for r.Next() {
		line := r.Value()
		row := make([]state, len(line))

		for i, c := range line {
			switch c {
			case '.':
				row[i] = floor
			case 'L':
				row[i] = empty
			default:
				panic(line)
			}
		}

		arr = append(arr, row)
	}

	return grid{arr, len(arr[0]), len(arr)}
}
