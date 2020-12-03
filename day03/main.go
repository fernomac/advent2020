package main

import (
	"fmt"

	"github.com/fernomac/advent2020/lib"
)

func main() {
	slope := load()

	// Part one: number of trees on slope {3, 1}
	fmt.Println(slope.check(3, 1))

	// Part two: the product of the number of trees on each slope.
	angles := [][]int{
		{1, 1},
		{3, 1},
		{5, 1},
		{7, 1},
		{1, 2},
	}

	product := 1
	for _, angle := range angles {
		trees := slope.check(angle[0], angle[1])
		product *= trees
	}

	fmt.Println(product)
}

type grid struct {
	trees [][]bool
}

// Load loads the grid from the input text file.
func load() *grid {
	trees := [][]bool{}

	lines := lib.ReadLines("input.txt")
	for lines.Next() {
		line := lines.Value()

		row := []bool{}
		for _, c := range line {
			row = append(row, c == '#')
		}

		trees = append(trees, row)
	}

	return &grid{trees}
}

// Check checks the number of trees passed on a run at the given slope.
func (g *grid) check(dx, dy int) int {
	x, y := 0, 0
	trees := 0

	for y < len(g.trees) {
		if g.tree(x, y) {
			trees++
		}
		x += dx
		y += dy
	}

	return trees
}

// Tree returns true if there is a tree at the given position.
func (g *grid) tree(x, y int) bool {
	row := g.trees[y]
	return row[x%len(row)]
}
