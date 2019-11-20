package main

import (
	"bufio"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"math"
	"os"
	"strings"

	"bytes"

	"github.com/bbatsov/solarized-emacs/colorlab/pkg/colorlab"
	"github.com/bbatsov/solarized-emacs/colorlab/pkg/colors"
	colorful "github.com/lucasb-eyer/go-colorful"
)

// Options are command line flags .
type Options struct {
	NoElispUpdate bool
}

func (o *Options) Register() {
	flag.BoolVar(&o.NoElispUpdate, "no-update", false, "don't update ../solarized-palettes.el")

}

func main() {
	var opts Options

	opts.Register()
	flag.Parse()

	for _, pal := range palettes {
		fmt.Println(pal.Name)
		nc := pal.Generate()
		nc.PrintAlist(os.Stdout, 0)
		// nc.FilterPrefix("base").PrintAlist(os.Stdout, 0)
		// Merge(nc.FilterSuffix("-d"), oldDarkAccents.NamedColors().WithSuffix("-do")).PrintAlist(os.Stdout, 0)
		// Merge(nc.FilterSuffix("-l"), oldLightAccents.NamedColors().WithSuffix("-lo")).PrintAlist(os.Stdout, 0)
		if !opts.NoElispUpdate {
			rewriteTheme(nc, pal.Name)
		}
	}
	fmt.Println("\n-----\n")
}

var palettes = []Palette{
	{
		Name:      "solarized-dark",
		Solarized: solarized,
		Accent1Pair: AccentPairGenerator{
			BlendBackgroundAmout:       default1BgFg.BlendBackgroundAmout,
			BlendForegroundAmout:       default1BgFg.BlendForegroundAmout,
			Gamma:                      default1BgFg.Gamma,
			ForegroundBlendFinder:      colorlab.NamedColorFinder("base1"),
			BackgroundBlendFinder:      default1BgFg.BackgroundBlendFinder,
			MinimumLightnessDifference: 0.42,
		},
		Accent2Pair: AccentPairGenerator{
			BlendBackgroundAmout:       default2BgFg.BlendBackgroundAmout,
			BlendForegroundAmout:       default2BgFg.BlendForegroundAmout,
			Gamma:                      default2BgFg.Gamma,
			ForegroundBlendFinder:      colorlab.NamedColorFinder("base1"),
			BackgroundBlendFinder:      default2BgFg.BackgroundBlendFinder,
			MinimumLightnessDifference: 0.42,
		},
	},
	{
		Name:        "solarized-light",
		Solarized:   solarized,
		Inverse:     true,
		Accent1Pair: default1BgFg,
		Accent2Pair: default2BgFg,
	},
	{
		Name:      "solarized-dark-high-contrast",
		Solarized: solarizedDarkHighContrast,
		Accent1Pair: AccentPairGenerator{
			BlendBackgroundAmout:       default1BgFg.BlendBackgroundAmout,
			BlendForegroundAmout:       default1BgFg.BlendForegroundAmout,
			Gamma:                      default1BgFg.Gamma,
			ForegroundBlendFinder:      colorlab.NamedColorFinder("base1"),
			BackgroundBlendFinder:      default1BgFg.BackgroundBlendFinder,
			MinimumLightnessDifference: 0.42,
		},
		Accent2Pair: AccentPairGenerator{
			BlendBackgroundAmout:       default2BgFg.BlendBackgroundAmout,
			BlendForegroundAmout:       default2BgFg.BlendForegroundAmout,
			Gamma:                      default2BgFg.Gamma,
			ForegroundBlendFinder:      colorlab.NamedColorFinder("base1"),
			BackgroundBlendFinder:      default2BgFg.BackgroundBlendFinder,
			MinimumLightnessDifference: 0.42,
		},
	},
	{
		Name:        "solarized-light-high-contrast",
		Solarized:   solarizedLightHighContrast,
		Inverse:     true,
		Accent1Pair: default1BgFg,
		Accent2Pair: default2BgFg,
	},
	{
		Name:        "gruvbox-dark",
		Solarized:   gruvboxDark,
		Accent1Pair: default1BgFg,
		Accent2Pair: default2BgFg,
	},
	{
		Name:        "gruvbox-light",
		Solarized:   gruvboxLight,
		Inverse:     true,
		Accent1Pair: default1BgFg,
		Accent2Pair: default2BgFg,
	},
	{
		Name:        "zenburn",
		Solarized:   zenburn,
		Accent1Pair: default1BgFg,
		Accent2Pair: default2BgFg,
	},
	{
		Name:        "monokai",
		Solarized:   monokai,
		Accent1Pair: default1BgFg,
		Accent2Pair: default2BgFg,
	},
}

var (
	default1BgFg = AccentPairGenerator{
		BlendBackgroundAmout:       0.85,
		BlendForegroundAmout:       0.3,
		Gamma:                      0.01,
		MinimumLightnessDifference: 0.35,
		ForegroundBlendFinder:      colorlab.ExtremeColorFgFinder,
		BackgroundBlendFinder:      colorlab.ExtremeColorBgFinder,
	}
	default2BgFg = AccentPairGenerator{
		BlendBackgroundAmout:       0.6,
		BlendForegroundAmout:       0.45,
		Gamma:                      0.04,
		MinimumLightnessDifference: 0.35,
		ForegroundBlendFinder:      colorlab.ExtremeColorFgFinder,
		BackgroundBlendFinder:      colorlab.ExtremeColorBgFinder,
	}
)

func (p Palette) Generate() colorlab.NamedColors {

	pal := p.Solarized

	corrected := pal
	if p.Inverse {
		corrected = pal.Inverse()
	}

	bgs, fgs := p.Accent1Pair.Generate(corrected)
	hbgs, hfgs := p.Accent2Pair.Generate(corrected)
	cols := colorlab.Merge(
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

// AccentPairGenerator generates paris of background and foreground colors that are meant to be used in pairs.
type AccentPairGenerator struct {
	BlendBackgroundAmout       float64
	BlendForegroundAmout       float64
	Gamma                      float64
	MinimumLightnessDifference float64

	BackgroundBlendFinder colorlab.ColorFinder
	ForegroundBlendFinder colorlab.ColorFinder
}

func (g *AccentPairGenerator) Generate(sol colorlab.Solarized) (backgrounds, foregrounds colorlab.Accents) {
	lightnessReport := false
	sol = sol.Clone()
	cs := sol.Accents.Colors()

	light := func(c colorful.Color) float64 {
		l, _, _ := c.Lab()
		return l
	}
	var bgs, fgs [8]colorful.Color
	for idx, v := range cs {
		if lightnessReport {
			fmt.Println(" ")
		}

		blendBgColor := g.BackgroundBlendFinder(sol)
		blendFgColor := g.ForegroundBlendFinder(sol)

		bg := v.BlendLab(blendBgColor, g.BlendBackgroundAmout)
		fg := v.BlendLab(blendFgColor, g.BlendForegroundAmout)
		if lightnessReport {
			{
				l, _, _ := blendBgColor.Lab()
				fmt.Printf(" | b1 %.2f %s", l, sol.Base03)
			}
			{
				l, _, _ := blendFgColor.Lab()
				fmt.Printf(" f1 %.2f %s", l, sol.Base0)
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

		const minimumLightnessDiff = 0.35
		// const minimumLightnessDiff = 0.45
		if (math.Max(nlf, nlb) - math.Min(nlf, nlb)) < minimumLightnessDiff {
			diff := (minimumLightnessDiff - (math.Max(math.Abs(nlf), math.Abs(nlb)) - math.Min(math.Abs(nlf), math.Abs(nlb))))
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
	return colorlab.NewAccents(bgs), colorlab.NewAccents(fgs)
}

var ( // The original solarized color palette
	solarized = colorlab.Solarized{
		Base: colorlab.Base{
			Base03: colors.SolarizedBase03,
			Base02: colors.SolarizedBase02,
			Base01: colors.SolarizedBase01,
			Base00: colors.SolarizedBase00,
			Base0:  colors.SolarizedBase0,
			Base1:  colors.SolarizedBase1,
			Base2:  colors.SolarizedBase2,
			Base3:  colors.SolarizedBase3,
		},
		Accents: colorlab.Accents{
			Blue:    colors.SolarizedBlue,
			Cyan:    colors.SolarizedCyan,
			Green:   colors.SolarizedGreen,
			Magenta: colors.SolarizedMagenta,
			Orange:  colors.SolarizedOrange,
			Red:     colors.SolarizedRed,
			Violet:  colors.SolarizedViolet,
			Yellow:  colors.SolarizedYellow,
		},
	}
	solarizedDarkHighContrast = colorlab.Solarized{
		Base:    solarized.Base.Clone().ChangeLightness(0.04, -0.02),
		Accents: solarized.Accents.Clone().ChangeLightness(0.05),
	}
	solarizedLightHighContrast = colorlab.Solarized{
		Base:    solarized.Base.Clone().ChangeLightness(0.02, -0.05),
		Accents: solarized.Accents.Clone().ChangeLightness(-0.05),
	}

	// the current -d colors in the emacs theme
	oldDarkAccents = colorlab.Accents{
		Blue:    "#00629D",
		Cyan:    "#00736F",
		Green:   "#546E00",
		Magenta: "#93115C",
		Orange:  "#8B2C02",
		Red:     "#990A1B",
		Violet:  "#3F4D91",
		Yellow:  "#7B6000",
	}
	// the current -l colors in the emacs theme
	oldLightAccents = colorlab.Accents{
		Blue:    "#69B7F0",
		Cyan:    "#69CABF",
		Green:   "#B4C342",
		Magenta: "#F771AC",
		Orange:  "#F2804F",
		Red:     "#FF6E64",
		Violet:  "#9EA0E5",
		Yellow:  "#DEB542",
	}
	gruvboxDark = colorlab.Solarized{
		Base: colorlab.Base{
			Base03: colors.GruvboxDark0,
			Base02: colors.GruvboxDark0Soft,
			Base01: colors.GruvboxDark4,
			Base00: colors.GruvboxDark0,
			Base0:  colors.GruvboxLight4,
			Base1:  colors.GruvboxLight3,
			Base2:  colors.GruvboxLight4,
			Base3:  colors.GruvboxLight0,
		},
		Accents: colorlab.Accents{
			Blue:    colors.GruvboxBlue,
			Cyan:    colors.GruvboxAqua,
			Green:   colors.GruvboxGreen,
			Magenta: colors.GruvboxBrightPurple,
			Orange:  colors.GruvboxOrange,
			Red:     colors.GruvboxBrightRed,
			Violet:  colors.GruvboxPurple,
			Yellow:  colors.GruvboxYellow,
		},
	}
	// note: not inversed here
	gruvboxLight = colorlab.Solarized{
		Base: colorlab.Base{
			Base03: colors.GruvboxDark0,
			Base02: colors.GruvboxDark0Soft,
			Base01: colors.GruvboxDark3,
			Base00: colors.GruvboxDark4,
			Base0:  colors.GruvboxDark1,
			Base1:  colors.GruvboxLight4,
			Base2:  colors.GruvboxLight1,
			Base3:  colors.GruvboxLight0,
		},
		Accents: colorlab.Accents{
			Blue:    colors.GruvboxDarkBlue,
			Cyan:    colors.GruvboxAqua,
			Green:   colors.GruvboxGreen,
			Magenta: colors.GruvboxBrightPurple,
			Orange:  colors.GruvboxDarkOrange,
			Red:     colors.GruvboxDarkRed,
			Violet:  colors.GruvboxDarkPurple,
			Yellow:  colors.GruvboxDarkYellow,
		},
	}
	zenburn = colorlab.Solarized{
		Base: colorlab.Base{
			Base03: colors.ZenburnBg,
			Base02: colors.ZenburnBgP1,
			Base01: colorlab.HexColor(colors.ZenburnFgM1).Blend(colors.ZenburnFg, 0.3),
			Base00: colors.ZenburnBgP3,
			Base0:  colors.ZenburnFg,
			Base1:  colors.ZenburnFgP1,
			Base2:  colorlab.HexColor(colors.ZenburnFgP1).Blend(colors.ZenburnFgP2, 0.5),
			Base3:  colors.ZenburnFgP2,
		},
		Accents: colorlab.Accents{
			Blue:    colors.ZenburnBlue,
			Cyan:    colors.ZenburnCyan,
			Green:   colors.ZenburnGreen,
			Magenta: colors.ZenburnMagenta,
			Orange:  colors.ZenburnOrange,
			Red:     colors.ZenburnRed,
			Violet:  colorlab.HexColor(colors.ZenburnBlue).Blend(colors.ZenburnMagenta, 0.5),
			Yellow:  colors.ZenburnYellow,
		},
	}

	monokai = colorlab.Solarized{
		Base: colorlab.Base{
			Base03: colors.Monokai03,
			Base02: colors.Monokai02,
			Base01: colors.Monokai01,
			Base00: colors.Monokai00,
			Base0:  colors.Monokai0,
			Base1:  colors.Monokai0,
			Base2:  colors.Monokai0,
			Base3:  colors.Monokai0,
		},
		Accents: colorlab.Accents{
			Blue:    colors.MonokaiBlue,
			Cyan:    colors.MonokaiCyan,
			Green:   colors.MonokaiGreen,
			Magenta: colors.MonokaiMagenta,
			Orange:  colors.MonokaiOrange,
			Red:     colors.MonokaiRed,
			Violet:  colors.MonokaiViolet,
			Yellow:  colors.MonokaiYellow,
		},
	}
)

// Palette .
type Palette struct {
	Name      string
	Inverse   bool
	Solarized colorlab.Solarized

	Accent1Pair AccentPairGenerator
	Accent2Pair AccentPairGenerator
}

func rewriteTheme(nc colorlab.NamedColors, paletteName string) {

	var dst bytes.Buffer

	file, err := os.Open("../solarized-palettes.el")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var insideReplacement bool
	for scanner.Scan() {
		txt := scanner.Text()
		if insideReplacement && strings.HasSuffix(txt, ";; palette end") {
			insideReplacement = false
		}
		if !insideReplacement {
			dst.WriteString(txt)
			dst.WriteString("\n")

		}
		if strings.HasSuffix(txt, fmt.Sprintf(";; %s palette", paletteName)) {
			insideReplacement = true
			nc.PrintAlist(&dst, 4)
		}
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	file.Close()

	ioutil.WriteFile("../solarized-palettes.el", dst.Bytes(), 0x776)

}
