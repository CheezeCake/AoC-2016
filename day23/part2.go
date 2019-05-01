package main

import "fmt"

func main() {
	a := 12
	b := a - 1
	c := 0
	d := 0
	toggled := map[int]bool{}
inst2:
	// 2
	d = a
	// 4
	a = b * d
	c = 0
	d = 0
	// 10
	b--
	c = b
	c *= 2
	d = 0
	// 16
	if _, present := toggled[c+16]; present {
		delete(toggled, c+16)
	} else {
		toggled[c+16] = true
	}
	// 17 & 18
	if _, present := toggled[18]; !present {
		c = -16
		goto inst2
	}
	a += 78 * 83
	d = 0
	c = 0
	fmt.Println("part 2:", a)
}
