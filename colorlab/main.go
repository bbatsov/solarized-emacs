package main

import (
	"flag"
	"fmt"
	"os"

	"github.com/bbatsov/solarized-emacs/colorlab/pkg/clab"
	"github.com/bbatsov/solarized-emacs/colorlab/pkg/colors"
	"github.com/bbatsov/solarized-emacs/colorlab/pkg/emacs"
	"github.com/bbatsov/solarized-emacs/colorlab/pkg/generator"
	"github.com/bbatsov/solarized-emacs/colorlab/pkg/sol"
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
		emacs.PrintAlist(os.Stdout, nc, 0)
		// nc.FilterPrefix("base").PrintAlist(os.Stdout, 0)
		// Merge(nc.FilterSuffix("-d"), oldDarkAccents.NamedColors().WithSuffix("-do")).PrintAlist(os.Stdout, 0)
		// Merge(nc.FilterSuffix("-l"), oldLightAccents.NamedColors().WithSuffix("-lo")).PrintAlist(os.Stdout, 0)
		if !opts.NoElispUpdate {
			emacs.RewritePalette(nc, pal.Name)
		}
	}
	fmt.Println("\n-----\n")
}

var (
	default1BgFg = generator.AccentPairGenerator{
		BlendBackgroundAmout:       0.85,
		BlendForegroundAmout:       0.3,
		Gamma:                      0.01,
		MinimumLightnessDifference: 0.35,
		ForegroundBlendFinder:      clab.ExtremeColorFgFinder,
		BackgroundBlendFinder:      clab.ExtremeColorBgFinder,
	}
	default2BgFg = generator.AccentPairGenerator{
		BlendBackgroundAmout:       0.6,
		BlendForegroundAmout:       0.45,
		Gamma:                      0.04,
		MinimumLightnessDifference: 0.35,
		ForegroundBlendFinder:      clab.ExtremeColorFgFinder,
		BackgroundBlendFinder:      clab.ExtremeColorBgFinder,
	}

	palettes = []generator.Palette{
		{
			Name:      "solarized-dark",
			Solarized: solarized,
			Accent1Pair: generator.AccentPairGenerator{
				BlendBackgroundAmout:       default1BgFg.BlendBackgroundAmout,
				BlendForegroundAmout:       default1BgFg.BlendForegroundAmout,
				Gamma:                      default1BgFg.Gamma,
				ForegroundBlendFinder:      clab.NamedColorFinder("base1"),
				BackgroundBlendFinder:      default1BgFg.BackgroundBlendFinder,
				MinimumLightnessDifference: 0.4,
			},
			Accent2Pair: generator.AccentPairGenerator{
				BlendBackgroundAmout:       default2BgFg.BlendBackgroundAmout,
				BlendForegroundAmout:       default2BgFg.BlendForegroundAmout,
				Gamma:                      default2BgFg.Gamma,
				ForegroundBlendFinder:      clab.NamedColorFinder("base1"),
				BackgroundBlendFinder:      default2BgFg.BackgroundBlendFinder,
				MinimumLightnessDifference: 0.4,
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
			Accent1Pair: generator.AccentPairGenerator{
				BlendBackgroundAmout:       default1BgFg.BlendBackgroundAmout,
				BlendForegroundAmout:       default1BgFg.BlendForegroundAmout,
				Gamma:                      default1BgFg.Gamma,
				ForegroundBlendFinder:      clab.NamedColorFinder("base1"),
				BackgroundBlendFinder:      default1BgFg.BackgroundBlendFinder,
				MinimumLightnessDifference: 0.4,
			},
			Accent2Pair: generator.AccentPairGenerator{
				BlendBackgroundAmout:       default2BgFg.BlendBackgroundAmout,
				BlendForegroundAmout:       default2BgFg.BlendForegroundAmout,
				Gamma:                      default2BgFg.Gamma,
				ForegroundBlendFinder:      clab.NamedColorFinder("base1"),
				BackgroundBlendFinder:      default2BgFg.BackgroundBlendFinder,
				MinimumLightnessDifference: 0.4,
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

	// The original solarized color palette
	solarized = sol.Solarized{
		Base: sol.Base{
			Base03: colors.SolarizedBase03,
			Base02: colors.SolarizedBase02,
			Base01: colors.SolarizedBase01,
			Base00: colors.SolarizedBase00,
			Base0:  colors.SolarizedBase0,
			Base1:  colors.SolarizedBase1,
			Base2:  colors.SolarizedBase2,
			Base3:  colors.SolarizedBase3,
		},
		Accents: sol.Accents{
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
	solarizedDarkHighContrast = sol.Solarized{
		Base:    solarized.Base.Clone().ChangeLightness(0.04, -0.02),
		Accents: solarized.Accents.Clone().ChangeLightness(0.05),
	}
	solarizedLightHighContrast = sol.Solarized{
		Base:    solarized.Base.Clone().ChangeLightness(0.02, -0.05),
		Accents: solarized.Accents.Clone().ChangeLightness(-0.05),
	}

	// the current -d colors in the emacs theme
	oldDarkAccents = sol.Accents{
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
	oldLightAccents = sol.Accents{
		Blue:    "#69B7F0",
		Cyan:    "#69CABF",
		Green:   "#B4C342",
		Magenta: "#F771AC",
		Orange:  "#F2804F",
		Red:     "#FF6E64",
		Violet:  "#9EA0E5",
		Yellow:  "#DEB542",
	}
	gruvboxDark = sol.Solarized{
		Base: sol.Base{
			Base03: colors.GruvboxDark0,
			Base02: colors.GruvboxDark0Soft,
			Base01: colors.GruvboxDark4,
			Base00: colors.GruvboxDark0,
			Base0:  colors.GruvboxLight4,
			Base1:  colors.GruvboxLight3,
			Base2:  colors.GruvboxLight4,
			Base3:  colors.GruvboxLight0,
		},
		Accents: sol.Accents{
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
	gruvboxLight = sol.Solarized{
		Base: sol.Base{
			Base03: colors.GruvboxDark0,
			Base02: colors.GruvboxDark0Soft,
			Base01: colors.GruvboxDark3,
			Base00: colors.GruvboxDark4,
			Base0:  colors.GruvboxDark1,
			Base1:  colors.GruvboxLight4,
			Base2:  colors.GruvboxLight1,
			Base3:  colors.GruvboxLight0,
		},
		Accents: sol.Accents{
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
	zenburn = sol.Solarized{
		Base: sol.Base{
			Base03: colors.ZenburnBg,
			Base02: colors.ZenburnBgP1,
			Base01: clab.HexColor(colors.ZenburnFgM1).Blend(colors.ZenburnFg, 0.3),
			Base00: colors.ZenburnBgP3,
			Base0:  colors.ZenburnFg,
			Base1:  colors.ZenburnFgP1,
			Base2:  clab.HexColor(colors.ZenburnFgP1).Blend(colors.ZenburnFgP2, 0.5),
			Base3:  colors.ZenburnFgP2,
		},
		Accents: sol.Accents{
			Blue:    colors.ZenburnBlue,
			Cyan:    colors.ZenburnCyan,
			Green:   colors.ZenburnGreen,
			Magenta: colors.ZenburnMagenta,
			Orange:  colors.ZenburnOrange,
			Red:     colors.ZenburnRed,
			Violet:  clab.HexColor(colors.ZenburnBlue).Blend(colors.ZenburnMagenta, 0.5),
			Yellow:  colors.ZenburnYellow,
		},
	}

	monokai = sol.Solarized{
		Base: sol.Base{
			Base03: colors.Monokai03,
			Base02: colors.Monokai02,
			Base01: colors.Monokai01,
			Base00: colors.Monokai00,
			Base0:  colors.Monokai0,
			Base1:  colors.Monokai0,
			Base2:  colors.Monokai0,
			Base3:  colors.Monokai0,
		},
		Accents: sol.Accents{
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
