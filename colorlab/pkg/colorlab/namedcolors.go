package colorlab

import (
	"fmt"
	"strings"
)

type NamedColors map[string]HexColor

// WithPrefix returns a copy of named colors where all keys are prefixed.
func (n NamedColors) WithPrefix(prefix string) NamedColors {
	nc := make(NamedColors, len(n))
	for k, v := range n {
		nc[fmt.Sprintf("%s%s", prefix, k)] = v
	}
	return nc
}

// WithSuffix returns a copy of named colors where all keys are suffixed.
func (n NamedColors) WithSuffix(suffix string) NamedColors {
	nc := make(NamedColors, len(n))
	for k, v := range n {
		nc[fmt.Sprintf("%s%s", k, suffix)] = v
	}
	return nc
}

func (n NamedColors) FilterSuffix(suffix ...string) NamedColors {
	nc := make(NamedColors, len(n))
	for k, v := range n {
	loop:
		for _, s := range suffix {
			if strings.HasSuffix(k, s) {
				nc[k] = v
				break loop
			}
		}
	}
	return nc
}

func (n NamedColors) FilterPrefix(prefix ...string) NamedColors {
	nc := make(NamedColors, len(n))
	for k, v := range n {
	loop:
		for _, p := range prefix {
			if strings.HasPrefix(k, p) {
				nc[k] = v
				break loop
			}
		}
	}
	return nc
}

func (n NamedColors) ColorList() ColorList {
	var res ColorList
	for _, v := range n {
		res = append(res, v.Color())

	}
	return res
}

// Merge merges multiple NamedColors
func Merge(nc ...NamedColors) NamedColors {
	res := make(NamedColors)
	for _, nc := range nc {
		for k, v := range nc {
			res[k] = v
		}
	}
	return res
}
