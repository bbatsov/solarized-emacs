package colorlab

import (
	"log"

	"github.com/lucasb-eyer/go-colorful"
)

// Find a specific color in a Solarized based on some preconditon
type ColorFinder func(NamedColors) colorful.Color

// NamedColorFinder extracts a named color from a palette
func NamedColorFinder(name string) ColorFinder {
	return func(colors NamedColors) colorful.Color {
		c, ok := colors[name]
		if !ok {
			log.Fatalf("color %s not in list: %v", name, colors)
		}
		return c.Color()
	}
}

// BgFinder returns base03 from the input Solarized
func BgFinder(nc NamedColors) colorful.Color {
	return NamedColorFinder("base03")(nc)

}

// FgFinder finds base0 from the input Solarized
func FgFinder(nc NamedColors) colorful.Color {
	return NamedColorFinder("base0")(nc)

}

// ExtremeColorFgFinder finds the darkest base tone for light on dark or the lightest color for dark on light.
func ExtremeColorFgFinder(nc NamedColors) colorful.Color {
	if nc.IsDarkOnBright("base03", "base0") {
		return nc.ColorList().ByLightnessDesc()[0]
	}
	return nc.ColorList().ByLightness()[0]
}

// ExtremeColorFgFinder finds the lightest base tone for light on dark or the darkest color for dark on light.
func ExtremeColorBgFinder(nc NamedColors) colorful.Color {
	if nc.IsDarkOnBright("base03", "base0") {
		return nc.ColorList().ByLightness()[0]
	}
	return nc.ColorList().ByLightnessDesc()[0]
}
