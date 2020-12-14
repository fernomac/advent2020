package main

import (
	"fmt"
	"sort"

	"github.com/fernomac/advent2020/lib"
)

func main() {
	ints := lib.LoadInts("input.txt")
	sort.Ints(ints)

	partOne(ints)
	partTwo(ints)
}

func partOne(ints []int) {
	diffs := [4]int{}

	diffs[ints[0]]++

	for i := 1; i < len(ints); i++ {
		diffs[ints[i]-ints[i-1]]++
	}

	diffs[3]++

	fmt.Println(diffs[1] * diffs[3])
}

func partTwo(ints []int) {
	fmt.Println(paths(0, ints, map[int]int{}))
}

func paths(from int, ints []int, memoize map[int]int) int {
	if val, ok := memoize[from]; ok {
		return val
	}

	if len(ints) == 0 {
		// Always exactly one path from the last adapter to the device.
		return 1
	}

	i := 0
	ret := 0
	for {
		if i >= len(ints) || ints[i]-from > 3 {
			memoize[from] = ret
			return ret
		}
		ret += paths(ints[i], ints[i+1:], memoize)
		i++
	}
}
