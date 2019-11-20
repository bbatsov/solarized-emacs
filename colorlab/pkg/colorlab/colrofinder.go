package colorlab

import "github.com/lucasb-eyer/go-colorful"

// Find a specific color in a Solarized based on some preconditon
type ColorFinder func(Solarized) colorful.Color

// DefaultBlendColorFgFinder finds the darkest base tone for light on dark or the lightest color for dark on light.
func DefaultBlendColorFgFinder(sol Solarized) colorful.Color {
	if sol.IsDarkOnBright() {
		return sol.Base.NamedColors().ColorList().ByLightnessDesc()[0]
	}
	return sol.Base.NamedColors().ColorList().ByLightness()[0]
}

// DefaultBlendColorFgFinder finds the lightest base tone for light on dark or the darkest color for dark on light.
func DefaultBlendColorBgFinder(sol Solarized) colorful.Color {
	if sol.IsDarkOnBright() {
		return sol.Base.NamedColors().ColorList().ByLightness()[0]
	}
	return sol.Base.NamedColors().ColorList().ByLightnessDesc()[0]
}
