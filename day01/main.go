package main

import (
	"fmt"

	"github.com/fernomac/advent2020/lib"
)

type pair struct {
	k, j int
}

func main() {
	ints := lib.ReadInts("input.txt")
	defer ints.Close()

	seen := make(map[int]struct{})
	sums := make(map[int]pair)

	for ints.Next() {
		i := ints.Value()

		// Part one: check if we have previously seen an int ii such that i+ii = 2020. If so we've found
		// a match: print the product of i and ii.
		ii := 2020 - i
		if _, ok := seen[ii]; ok {
			p := i * ii
			fmt.Printf("%[1]v + %[2]v = 2020;\t  %[1]v * %[2]v = %[3]v\n", ii, i, p)
		}

		// Part two: check if we have previously seen a _pair_ of ints {j, k} that sum to ii. If so we've
		// found a match: print the product of i, j, and k.
		if jk, ok := sums[ii]; ok {
			p := i * jk.j * jk.k
			fmt.Printf("%[1]v + %[2]v + %[3]v = 2020;\t  %[1]v * %[2]v * %[3]v = %[4]v\n", jk.k, jk.j, i, p)
		}

		// Remember the sum of i and every previously-seen j for part two.
		for j := range seen {
			if i+j < 2020 {
				sums[i+j] = pair{j, i}
			}
		}

		// Remember i for part one.
		seen[i] = struct{}{}
	}
}
