package lib

import (
	"bufio"
	"os"
	"strconv"
)

// IntReader reads ints
type IntReader struct {
	r *LineReader
}

// ReadInts reads a file as a sequence of ints
func ReadInts(file string) *IntReader {
	return &IntReader{ReadLines(file)}
}

// LoadInts loads all of the ints from a file into memory at once.
func LoadInts(file string) []int {
	ret := []int{}
	r := ReadInts(file)
	for r.Next() {
		ret = append(ret, r.Value())
	}
	return ret
}

// Next moves to the next int
func (r *IntReader) Next() bool {
	return r.r.Next()
}

// Value returns the current value
func (r *IntReader) Value() int {
	i, err := strconv.Atoi(r.r.Value())
	if err != nil {
		panic(err)
	}
	return i
}

// Close closes the reader.
func (r *IntReader) Close() error {
	return r.r.Close()
}

// LineReader reads lines
type LineReader struct {
	f *os.File
	s *bufio.Scanner
}

// ReadLines reads a file as a sequence of lines
func ReadLines(file string) *LineReader {
	f, err := os.Open(file)
	if err != nil {
		panic(err)
	}

	s := bufio.NewScanner(f)

	return &LineReader{f, s}
}

// Next moves to the next line if there is one
func (r *LineReader) Next() bool {
	ret := r.s.Scan()
	if r.s.Err() != nil {
		panic(r.s.Err())
	}
	return ret
}

// Value returns the current line
func (r *LineReader) Value() string {
	return r.s.Text()
}

// Close closes the reader
func (r *LineReader) Close() error {
	return r.f.Close()
}
