package main

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/fernomac/advent2020/lib"
)

func main() {
	r := lib.ReadLines("input.txt")
	graph := graph{}

	for r.Next() {
		bag, contents := parse(r.Value())
		graph[bag] = contents
	}

	// Part one: how many bag colors can contain at least one shiny bag?
	count := 0
	for bag := range graph {
		if graph.isGolden(bag) {
			count++
		}
	}
	fmt.Println(count)

	// Part two: how many bags fit inside?
	fmt.Println(graph.bagsInside("shiny gold"))
}

type edge struct {
	color string
	num   int
}

type graph map[string][]edge

func (g graph) isGolden(bag string) bool {
	return g.isGoldenHelper(bag, map[string]struct{}{})
}

func (g graph) isGoldenHelper(bag string, visited map[string]struct{}) bool {
	edges := g[bag]

	// Can we directly hold a shiny gold bag?
	for _, edge := range edges {
		if edge.color == "shiny gold" {
			return true
		}
	}

	// Can we indirectly hold a shiny gold bag?
	for _, edge := range edges {
		if _, ok := visited[edge.color]; !ok {
			visited[edge.color] = struct{}{}
			ok := g.isGoldenHelper(edge.color, visited)
			delete(visited, edge.color)

			if ok {
				return true
			}
		}
	}

	return false
}

func (g graph) bagsInside(bag string) int {
	edges := g[bag]

	bags := 0
	for _, edge := range edges {
		bags += edge.num * (1 + g.bagsInside(edge.color))
	}
	return bags
}

func contains(set []string, s string) bool {
	for _, i := range set {
		if i == s {
			return true
		}
	}
	return false
}

func parse(line string) (string, []edge) {
	idx := strings.Index(line, " bags contain ")
	color := line[:idx]
	rest := line[idx+14:]
	return color, parseContents(rest)
}

func parseContents(line string) []edge {
	if line == "no other bags." {
		return nil
	}

	ret := []edge{}
	for len(line) > 0 {
		idx := strings.Index(line, " bag")
		if idx < 0 {
			panic(line)
		}

		head := line[:idx]
		tail := line[idx+4:]

		tail = strings.TrimPrefix(tail, "s")
		tail = tail[1:] // , or .
		tail = strings.TrimPrefix(tail, " ")

		ret = append(ret, parseContent(head))
		line = tail
	}
	return ret
}

func parseContent(str string) edge {
	idx := strings.Index(str, " ")
	if idx < 0 {
		panic(str)
	}

	num, err := strconv.Atoi(str[:idx])
	if err != nil {
		panic(err)
	}

	return edge{str[idx+1:], num}
}
