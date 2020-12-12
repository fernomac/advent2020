package main

import (
	"fmt"

	"github.com/fernomac/advent2020/lib"
)

func main() {
	ints := lib.LoadInts("input.txt")

	mismatch := partOne(ints)
	fmt.Println(mismatch)

	sum := partTwo(ints, mismatch)
	fmt.Println(sum)
}

// Part one: find the first int that's NOT the sum of two of the previous 25 ints.
func partOne(ints []int) int {
	for i := 25; i < len(ints); i++ {
		if !canSumTo(ints[i-25:i], ints[i]) {
			return ints[i]
		}
	}
	panic("not found")
}

// Determine if any pair of ints in `ints` sum to `target`.
func canSumTo(ints []int, target int) bool {
	diffs := map[int]struct{}{}
	for _, i := range ints {
		diff := target - i
		if diff != i {
			if _, ok := diffs[diff]; ok {
				return true
			}
		}
		diffs[i] = struct{}{}
	}
	return false
}

// Part two: find a contiguous range of ints that sum to target.
func partTwo(ints []int, target int) int {
	mini, maxi := 0, 1
	for {
		is := ints[mini:maxi]
		sum := sum(is)
		switch {
		case sum == target:
			return min(is) + max(is)

		case sum < target:
			maxi++

		case sum > target:
			mini++
		}
	}
}

func min(ints []int) int {
	min := ints[0]
	for i := 1; i < len(ints); i++ {
		if ints[i] < min {
			min = ints[i]
		}
	}
	return min
}

func max(ints []int) int {
	max := ints[0]
	for i := 1; i < len(ints); i++ {
		if ints[i] > max {
			max = ints[i]
		}
	}
	return max
}

func sum(ints []int) int {
	sum := 0
	for _, i := range ints {
		sum += i
	}
	return sum
}
