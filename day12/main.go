package main

import (
	"fmt"
	"strconv"

	"github.com/fernomac/advent2020/lib"
)

func main() {
	lines := lib.ReadLines("input.txt")
	defer lines.Close()

	p1 := partOne{dir: east}
	p2 := partTwo{dir: vector{x: 10, y: -1}}

	// Parse the instructions and apply each to each movement strategy in parallel.
	for lines.Next() {
		line := lines.Value()

		action := line[0]
		num, err := strconv.Atoi(line[1:])
		if err != nil {
			panic(err)
		}

		p1.move(action, num)
		p2.move(action, num)
	}

	// Print the manhattan distances of the final ship positions.
	fmt.Println(abs(p1.pos.x) + abs(p1.pos.y))
	fmt.Println(abs(p2.pos.x) + abs(p2.pos.y))
}

// abs returns the absolute value of i.
func abs(i int) int {
	if i < 0 {
		return -i
	}
	return i
}

// Part one: NSEW translate the ship, LR turn it, F moves forward.
type partOne struct {
	pos vector
	dir vector
}

func (p *partOne) move(action byte, num int) {
	switch action {
	case 'N':
		p.pos = p.pos.plus(north.times(num))
	case 'S':
		p.pos = p.pos.plus(south.times(num))
	case 'E':
		p.pos = p.pos.plus(east.times(num))
	case 'W':
		p.pos = p.pos.plus(west.times(num))
	case 'L':
		p.dir = p.dir.rotate(360 - num)
	case 'R':
		p.dir = p.dir.rotate(num)
	case 'F':
		p.pos = p.pos.plus(p.dir.times(num))
	default:
		panic(fmt.Sprintf("invalid input: %v", action))
	}
}

// Part two: NSEW move the _target_, LR rotate it around the ship, F moves towards it.
type partTwo struct {
	pos vector
	dir vector
}

func (p *partTwo) move(action byte, num int) {
	switch action {
	case 'N':
		p.dir = p.dir.plus(north.times(num))
	case 'S':
		p.dir = p.dir.plus(south.times(num))
	case 'E':
		p.dir = p.dir.plus(east.times(num))
	case 'W':
		p.dir = p.dir.plus(west.times(num))
	case 'L':
		p.dir = p.dir.rotate(360 - num)
	case 'R':
		p.dir = p.dir.rotate(num)
	case 'F':
		p.pos = p.pos.plus(p.dir.times(num))
	default:
		panic(fmt.Sprintf("invalid input: %v", action))
	}
}

var (
	north = vector{x: 0, y: -1}
	south = vector{x: 0, y: 1}
	west  = vector{x: -1, y: 0}
	east  = vector{x: 1, y: 0}
)

// Vector is a two-dimensional vector.
type vector struct {
	x, y int
}

// Plus adds two vectors together.
func (v vector) plus(o vector) vector {
	return vector{v.x + o.x, v.y + o.y}
}

// Times multiplies a vector by a scalar.
func (v vector) times(i int) vector {
	return vector{v.x * i, v.y * i}
}

// Rotate rotates a vector by the given number of degrees.
func (v vector) rotate(deg int) vector {
	switch deg {
	case 0:
		return v
	case 90:
		return vector{-v.y, v.x}
	case 180:
		return vector{-v.x, -v.y}
	case 270:
		return vector{v.y, -v.x}
	default:
		panic(fmt.Sprintf("unsupported rotation: %v", deg))
	}
}
