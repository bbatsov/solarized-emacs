package generator

import (
	"fmt"
	"math"

	"github.com/bbatsov/solarized-emacs/colorlab/pkg/clab"
	"github.com/bbatsov/solarized-emacs/colorlab/pkg/sol"
	"github.com/lucasb-eyer/go-colorful"
)

// AccentPairGenerator generates paris of background and foreground colors that are meant to be used in pairs.
type AccentPairGenerator struct {
	BlendBackgroundAmout       float64
	BlendForegroundAmout       float64
	Gamma                      float64
	MinimumLightnessDifference float64

	BackgroundBlendFinder clab.ColorFinder
	ForegroundBlendFinder clab.ColorFinder
}

func (g *AccentPairGenerator) Generate(solarized sol.Solarized) (backgrounds, foregrounds sol.Accents) {
	lightnessReport := false
	solarized = solarized.Clone()
	cs := solarized.Accents.Colors()

	light := func(c colorful.Color) float64 {
		l, _, _ := c.Lab()
		return l
	}
	var bgs, fgs [8]colorful.Color
	for idx, v := range cs {
		if lightnessReport {
			fmt.Println(" ")
		}

		blendBgColor := g.BackgroundBlendFinder(solarized.Base.NamedColors())
		blendFgColor := g.ForegroundBlendFinder(solarized.Base.NamedColors())

		bg := v.BlendLab(blendBgColor, g.BlendBackgroundAmout)
		fg := v.BlendLab(blendFgColor, g.BlendForegroundAmout)
		if lightnessReport {
			{
				l, _, _ := blendBgColor.Lab()
				fmt.Printf(" | b1 %.2f %s", l, solarized.Base03)
			}
			{
				l, _, _ := blendFgColor.Lab()
				fmt.Printf(" f1 %.2f %s", l, solarized.Base0)
			}
		}

		lFg := light(blendFgColor)
		lBg := light(blendBgColor)

		lc := lFg - lBg
		if lightnessReport {
			fmt.Printf("  lc %.2f", lc)
		}
		bl, ba, bb := bg.Lab()
		fl, fa, fb := fg.Lab()

		if lightnessReport {
			fmt.Printf(" | b2 %.2f %s", bl, bg.Clamped().Hex())
			fmt.Printf(" f2 %.2f %s", fl, fg.Clamped().Hex())
			fmt.Printf(" lc %.2f", fl-bl)
		}
		// nlb := bl + (fl-bl)*0.5 + lc*0.5
		// nlf := bl + (fl-bl)*0.5 - lc*0.5
		// nlf := lFg
		// nlb := lBg

		nlb := bl
		nlf := fl
		if fl > bl {
			nlb = nlb - g.Gamma
			nlf = nlf - g.Gamma
		} else {
			nlb = nlb + g.Gamma
			nlf = nlf + g.Gamma
		}

		if (math.Max(nlf, nlb) - math.Min(nlf, nlb)) < g.MinimumLightnessDifference {
			diff := (g.MinimumLightnessDifference - (math.Max(math.Abs(nlf), math.Abs(nlb)) - math.Min(math.Abs(nlf), math.Abs(nlb))))
			m := 1.0
			if bl > fl {
				m = -1
			}
			nlf = nlf + nlf/1*diff*m
			nlb = nlb - nlb/1*diff*m
			if lightnessReport {
				fmt.Printf(" adj %.2f ", diff)
			}
		} else {
			if lightnessReport {
				fmt.Printf("          ")
			}
		}

		fg = colorful.Lab(nlf, fa, fb)
		bg = colorful.Lab(nlb, ba, bb)
		if lightnessReport {
			fmt.Printf(" | b3 %.2f %s", nlb, bg.Clamped().Hex())
			fmt.Printf(" f3 %.2f %s", nlf, fg.Clamped().Hex())
			fmt.Printf(" lc %.2f", nlf-nlb)
		}
		bgs[idx] = bg
		fgs[idx] = fg

	}
	if lightnessReport {
		fmt.Println("\n\n")
	}
	return sol.NewAccents(bgs), sol.NewAccents(fgs)
}
