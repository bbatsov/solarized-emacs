package generator

import (
	"github.com/bbatsov/solarized-emacs/colorlab/pkg/clab"
	"github.com/bbatsov/solarized-emacs/colorlab/pkg/sol"
)

// Palette .
type Palette struct {
	Name      string
	Inverse   bool
	Solarized sol.Solarized

	Accent1Pair AccentPairGenerator
	Accent2Pair AccentPairGenerator
}

func (p Palette) Generate() clab.NamedColors {

	pal := p.Solarized

	corrected := pal
	if p.Inverse {
		corrected = pal.Inverse()
	}

	bgs, fgs := p.Accent1Pair.Generate(corrected)
	hbgs, hfgs := p.Accent2Pair.Generate(corrected)
	cols := clab.Merge(
		pal.NamedColors(),
		fgs.NamedColors().WithSuffix("-1fg"),
		bgs.NamedColors().WithSuffix("-1bg"),
		hfgs.NamedColors().WithSuffix("-2fg"),
		hbgs.NamedColors().WithSuffix("-2bg"),

		pal.Accents.
			ChangeSaturation(0.2).
			Blend(pal.Base03, .2).
			ChangeLightness(-.1).
			NamedColors().
			WithSuffix("-d"),

		pal.Accents.
			ChangeSaturation(0.2).
			Blend(pal.Base2, .2).
			ChangeLightness(.05).
			NamedColors().
			WithSuffix("-l"),
	)
	return cols

}
