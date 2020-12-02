package main

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/fernomac/advent2020/lib"
)

// A line is single line of input from the text file.
type line struct {
	i, j     int
	char     byte
	password string
}

func main() {
	valid1 := 0
	valid2 := 0

	lines := lib.ReadLines("input.txt")
	for lines.Next() {
		pass := parse(lines.Value())
		if validate1(pass) {
			valid1++
		}
		if validate2(pass) {
			valid2++
		}
	}

	fmt.Println(valid1, valid2)
}

// Part one: a password is valid if the given char appears between i and j times.
func validate1(in line) bool {
	count := 0
	for i := 0; i < len(in.password); i++ {
		if in.password[i] == in.char {
			count++
		}
	}

	return count >= in.i && count <= in.j
}

// Part two: a password is valid if the given char appears in exactly one of the two
// listed positions (1-indexed).
func validate2(in line) bool {
	count := 0
	if in.password[in.i-1] == in.char {
		count++
	}
	if in.password[in.j-1] == in.char {
		count++
	}
	return count == 1
}

// Parse parses a line of input.
func parse(in string) line {
	parts := strings.Split(in, " ")
	if len(parts) != 3 {
		panic(in)
	}

	i, j := parseInts(parts[0])
	char := parts[1][0]
	password := parts[2]

	return line{i, j, char, password}
}

// ParseInts parses the two leading ints from a line of input.
func parseInts(in string) (int, int) {
	parts := strings.Split(in, "-")
	if len(parts) != 2 {
		panic(in)
	}

	i, err := strconv.Atoi(parts[0])
	if err != nil {
		panic(err)
	}

	j, err := strconv.Atoi(parts[1])
	if err != nil {
		panic(err)
	}

	return i, j
}
