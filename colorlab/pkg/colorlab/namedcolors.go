package colorlab

import (
	"fmt"
	"io"
	"sort"
	"strconv"
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

// PrintAlist prints the named colors list in a way that is compatible with solarized-palettes.el.
func (n NamedColors) PrintAlist(w io.Writer, indent int) (int, error) {
	keys := n.OrderedKeys()
	var longestKey int
	for _, n := range keys {
		l := len(n)
		if l > longestKey {
			longestKey = l
		}
	}
	longestKey = longestKey
	prefix := ""
	for i := 0; i < indent; i++ {
		prefix += " "

	}
	var nt int

	for _, name := range keys {
		n, err := fmt.Fprintf(w, "%s(%-"+strconv.Itoa(longestKey)+"s . \"%s\")\n", prefix, name, n[name])
		n = n + nt
		if err != nil {
			return nt, err
		}

	}
	return nt, nil
}

func (n NamedColors) ColorList() ColorList {
	var res ColorList
	for _, v := range n {
		res = append(res, v.Color())

	}
	return res
}

// Orderedkeys returns the keys in a order that is like how solarized names should be ordered.
//
// primary orer is:
//
// 1. base names
// 2. accent colors
// 3. names that contain base names
// 4. names that contain accent color names
// 5. the rest
//
// secondary sort order is by string comparison.
//
func (n NamedColors) OrderedKeys() []string {
	var names []string
	for k, _ := range n {
		names = append(names, k)
	}
	sort.Sort(sort.StringSlice(names))

	var allNames []string
	for _, n := range baseNames {
		allNames = append(allNames, n)
	}
	for _, n := range accentNames {
		allNames = append(allNames, n)
	}
	score := func(idx int, exact bool) int {
		s := (len(allNames) - idx) + 1
		if exact {
			s = s + len(allNames)
		}
		return s
	}
	sort.SliceStable(names, func(i int, j int) bool {
		var is, js int // scores
		in := names[i]
		jn := names[j]
	scoring:
		for sortIdx, sortName := range allNames {
			if is == 0 {
				// log.Println(in, sortName)
				if strings.Contains(in, sortName) {
					is = score(sortIdx, in == sortName)
					// fmt.Println("score in", in, sortName, is)
				}
			}
			if js == 0 {
				if strings.Contains(jn, sortName) {
					js = score(sortIdx, jn == sortName)
					// fmt.Println("score j", jn, sortName, js)
				}
			}
			if is != 0 && js != 0 {
				break scoring
			}
		}
		// log.Println(is, js, in,jn)
		return is > js
	})
	return names
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
