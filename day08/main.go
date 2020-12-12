package main

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/fernomac/advent2020/lib"
)

func main() {
	ops := []opcode{}
	lines := lib.ReadLines("input.txt")
	for lines.Next() {
		ops = append(ops, parseOpcode(lines.Value()))
	}

	// Part one: what is the value of acc when a loop is detected?
	m := newMachine(ops)
	m.Run()
	fmt.Println(m.acc)

	// Part two: change a nop->jmp or vice versa to make the program terminate.
	for i := 0; i < len(ops); i++ {
		if ops[i].code == "acc" {
			continue
		}

		nops := clone(ops)
		switch ops[i].code {
		case "jmp":
			nops[i].code = "nop"
		case "nop":
			nops[i].code = "jmp"
		default:
			panic(ops[i].code)
		}

		m := newMachine(nops)
		if terminated := m.Run(); terminated {
			fmt.Println(m.acc)
			break
		}
	}
}

func clone(ops []opcode) []opcode {
	ret := make([]opcode, len(ops))
	copy(ret, ops)
	return ret
}

type opcode struct {
	code string
	arg  int
}

func parseOpcode(line string) opcode {
	parts := strings.Split(line, " ")
	if len(parts) != 2 {
		panic(line)
	}

	arg, err := strconv.Atoi(parts[1])
	if err != nil {
		panic(fmt.Sprintf("%v: %v", err, line))
	}

	return opcode{parts[0], arg}
}

type machine struct {
	ops []opcode
	ip  int
	acc int

	hits map[int]struct{}
}

func newMachine(ops []opcode) *machine {
	return &machine{
		ops:  ops,
		hits: map[int]struct{}{},
	}
}

func (m *machine) Run() bool {
	for {
		done, terminated := m.step()
		if done {
			return terminated
		}
	}
}

func (m *machine) step() (bool, bool) {
	if _, ok := m.hits[m.ip]; ok {
		return true, false
	}
	m.hits[m.ip] = struct{}{}

	if m.ip == len(m.ops) {
		return true, true
	}

	op := m.ops[m.ip]
	switch op.code {
	case "nop":
		m.ip++

	case "acc":
		m.acc += op.arg
		m.ip++

	case "jmp":
		m.ip += op.arg

	default:
		panic(op.code)
	}

	return false, false
}
