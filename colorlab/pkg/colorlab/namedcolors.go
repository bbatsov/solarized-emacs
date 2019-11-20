package colorlab

import (
	"fmt"
	"log"
	"strings"

	"github.com/lucasb-eyer/go-colorful"
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

// Returns true if the LAB lightness of bgName is larger than fgName.
func (nc NamedColors) IsDarkOnBright(bgName, fgName string) bool {
	lightness := func(c colorful.Color) float64 {
		l, _, _ := c.Lab()
		return l
	}

	// bg, ok := nc["base03"]
	bg, ok := nc[bgName]
	if !ok {
		log.Fatalf("base03 not found in: ", nc)
	}

	// fg := nc["base0"]
	fg := nc[fgName]
	if !ok {
		log.Fatalf("base0 not found in: ", nc)
	}

	bgl := lightness(bg.Color())
	fgl := lightness(fg.Color())
	if fgl == bgl {
		log.Fatalf("bg (%v) and fg (%v) are not supposed to have equal lightness: %v %v", bg, fg, bgl, fgl)
	}
	return fgl > bgl
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
