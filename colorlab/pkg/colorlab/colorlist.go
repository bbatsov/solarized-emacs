package colorlab

import (
	"sort"

	"github.com/lucasb-eyer/go-colorful"
)

type ColorList []colorful.Color

// returns the ColorList ordered by Lightness, darkest first
func (c ColorList) ByLightness() ColorList {
	res := c
	sort.Sort(ByLightness(res))
	return res
}

// returns the ColorList ordered by Lightness, lightest first
func (c ColorList) ByLightnessDesc() ColorList {
	res := c
	sort.Sort(sort.Reverse(ByLightness(res)))
	return res
}

// ByLightness sorts a list of colors by LAB L(ightness) value
type ByLightness ColorList

func (a ByLightness) Len() int      { return len(a) }
func (a ByLightness) Swap(i, j int) { a[i], a[j] = a[j], a[i] }
func (a ByLightness) Less(i, j int) bool {
	lightness := func(c colorful.Color) float64 {
		l, _, _ := c.Lab()
		return l
	}
	return lightness(a[i]) < lightness(a[j])
}
