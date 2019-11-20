package colorlab

import (
	"log"

	"github.com/lucasb-eyer/go-colorful"
)

// Find a specific color in a Solarized based on some preconditon
type ColorFinder func(Solarized) colorful.Color

// NamedColorFinder extracts a named color from a palette
func NamedColorFinder(name string) ColorFinder {
	return func(sol Solarized) colorful.Color {
		colors := sol.NamedColors()
		c, ok := colors[name]
		if !ok {
			log.Fatalf("color %s not in list: %v", name, colors)
		}
		return c.Color()
	}
}

// BgFinder returns base03 from the input Solarized
func BgFinder(sol Solarized) colorful.Color {
	return NamedColorFinder("base03")(sol)

}

// FgFinder finds base0 from the input Solarized
func FgFinder(sol Solarized) colorful.Color {
	return NamedColorFinder("base0")(sol)

}

// ExtremeColorFgFinder finds the darkest base tone for light on dark or the lightest color for dark on light.
func ExtremeColorFgFinder(sol Solarized) colorful.Color {
	if sol.IsDarkOnBright() {
		return sol.Base.NamedColors().ColorList().ByLightnessDesc()[0]
	}
	return sol.Base.NamedColors().ColorList().ByLightness()[0]
}

// ExtremeColorFgFinder finds the lightest base tone for light on dark or the darkest color for dark on light.
func ExtremeColorBgFinder(sol Solarized) colorful.Color {
	if sol.IsDarkOnBright() {
		return sol.Base.NamedColors().ColorList().ByLightness()[0]
	}
	return sol.Base.NamedColors().ColorList().ByLightnessDesc()[0]
}
