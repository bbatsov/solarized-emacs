package main

import (
	"bufio"
	"bytes"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os"
	"strconv"
	"strings"

	cl "github.com/go-pa/colorlab"
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
		PrintAlist(os.Stdout, nc, 0)
		// nc.FilterPrefix("base").PrintAlist(os.Stdout, 0)
		// Merge(nc.FilterSuffix("-d"), oldDarkAccents.NamedColors().WithSuffix("-do")).PrintAlist(os.Stdout, 0)
		// Merge(nc.FilterSuffix("-l"), oldLightAccents.NamedColors().WithSuffix("-lo")).PrintAlist(os.Stdout, 0)
		if !opts.NoElispUpdate {
			RewritePalette(nc, pal.Name)
		}
	}
	fmt.Println("\n-----\n")
}

var (
	default1BgFg = cl.AccentPairGenerator{
		BlendBackgroundAmout:       0.85,
		BlendForegroundAmout:       0.3,
		Gamma:                      0.01,
		MinimumLightnessDifference: 0.35,
		ForegroundBlendFinder:      cl.ExtremeColorFgFinder,
		BackgroundBlendFinder:      cl.ExtremeColorBgFinder,
	}
	default2BgFg = cl.AccentPairGenerator{
		BlendBackgroundAmout:       0.6,
		BlendForegroundAmout:       0.45,
		Gamma:                      0.04,
		MinimumLightnessDifference: 0.35,
		ForegroundBlendFinder:      cl.ExtremeColorFgFinder,
		BackgroundBlendFinder:      cl.ExtremeColorBgFinder,
	}

	palettes = []cl.Palette{
		{
			Name:      "solarized-dark",
			Solarized: solarized,
			Accent1Pair: cl.AccentPairGenerator{
				BlendBackgroundAmout:       default1BgFg.BlendBackgroundAmout,
				BlendForegroundAmout:       default1BgFg.BlendForegroundAmout,
				Gamma:                      default1BgFg.Gamma,
				ForegroundBlendFinder:      cl.NamedColorFinder("base1"),
				BackgroundBlendFinder:      default1BgFg.BackgroundBlendFinder,
				MinimumLightnessDifference: 0.4,
			},
			Accent2Pair: cl.AccentPairGenerator{
				BlendBackgroundAmout:       default2BgFg.BlendBackgroundAmout,
				BlendForegroundAmout:       default2BgFg.BlendForegroundAmout,
				Gamma:                      default2BgFg.Gamma,
				ForegroundBlendFinder:      cl.NamedColorFinder("base1"),
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
			Accent1Pair: cl.AccentPairGenerator{
				BlendBackgroundAmout:       default1BgFg.BlendBackgroundAmout,
				BlendForegroundAmout:       default1BgFg.BlendForegroundAmout,
				Gamma:                      default1BgFg.Gamma,
				ForegroundBlendFinder:      cl.NamedColorFinder("base1"),
				BackgroundBlendFinder:      default1BgFg.BackgroundBlendFinder,
				MinimumLightnessDifference: 0.4,
			},
			Accent2Pair: cl.AccentPairGenerator{
				BlendBackgroundAmout:       default2BgFg.BlendBackgroundAmout,
				BlendForegroundAmout:       default2BgFg.BlendForegroundAmout,
				Gamma:                      default2BgFg.Gamma,
				ForegroundBlendFinder:      cl.NamedColorFinder("base1"),
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
		{
			Name:        "selenized-dark",
			Solarized:   selenizedDark,
			Accent1Pair: default1BgFg,
			Accent2Pair: default2BgFg,
		},
		{
			Name:        "selenized-black",
			Solarized:   selenizedBlack,
			Accent1Pair: default1BgFg,
			Accent2Pair: default2BgFg,
		},
		{
			Name:        "selenized-light",
			Solarized:   selenizedLight,
			Accent1Pair: default1BgFg,
			Accent2Pair: default2BgFg,
		},
		{
			Name:        "selenized-white",
			Solarized:   selenizedWhite,
			Accent1Pair: default1BgFg,
			Accent2Pair: default2BgFg,
		},
		{
			Name:        "everforest-dark",
			Solarized:   everforestDark,
			Accent1Pair: default1BgFg,
			Accent2Pair: default2BgFg,
		},
		{
			Name:        "everforest-light",
			Solarized:   everforestLight,
			Accent1Pair: default1BgFg,
			Accent2Pair: default2BgFg,
		},
		{
			Name:        "flexoki-dark",
			Solarized:   flexokiDark,
			Accent1Pair: default1BgFg,
			Accent2Pair: default2BgFg,
		},
		{
			Name:        "flexoki-light",
			Solarized:   flexokiLight,
			Accent1Pair: default1BgFg,
			Accent2Pair: default2BgFg,
		},
		{
			Name:        "kanagawa-wave",
			Solarized:   kanagawaWave,
			Accent1Pair: default1BgFg,
			Accent2Pair: default2BgFg,
		},
		{
			Name:        "kanagawa-dragon",
			Solarized:   kanagawaDragon,
			Accent1Pair: default1BgFg,
			Accent2Pair: default2BgFg,
		},
		{
			Name:        "kanagawa-lotus",
			Solarized:   kanagawaLotus,
			Accent1Pair: default1BgFg,
			Accent2Pair: default2BgFg,
		},
		{
			Name:        "nightfox",
			Solarized:   nightfox,
			Accent1Pair: default1BgFg,
			Accent2Pair: default2BgFg,
		},
		{
			Name:        "dayfox",
			Solarized:   dayfox,
			Accent1Pair: default1BgFg,
			Accent2Pair: default2BgFg,
		},
		{
			Name:        "dawnfox",
			Solarized:   dawnfox,
			Accent1Pair: default1BgFg,
			Accent2Pair: default2BgFg,
		},
		{
			Name:        "duskfox",
			Solarized:   duskfox,
			Accent1Pair: default1BgFg,
			Accent2Pair: default2BgFg,
		},
		{
			Name:        "nordfox",
			Solarized:   nordfox,
			Accent1Pair: default1BgFg,
			Accent2Pair: default2BgFg,
		},
		{
			Name:        "terafox",
			Solarized:   terafox,
			Accent1Pair: default1BgFg,
			Accent2Pair: default2BgFg,
		},
		{
			Name:        "carbonfox",
			Solarized:   carbonfox,
			Accent1Pair: default1BgFg,
			Accent2Pair: default2BgFg,
		},
		{
			Name:        "carbon-dark",
			Solarized:   carbonDark,
			Accent1Pair: default1BgFg,
			Accent2Pair: default2BgFg,
		},
		{
			Name:        "carbon-light",
			Solarized:   carbonLight,
			Accent1Pair: default1BgFg,
			Accent2Pair: default2BgFg,
		},
		{
			Name:        "apple-dark",
			Solarized:   appleDark,
			Accent1Pair: default1BgFg,
			Accent2Pair: default2BgFg,
		},
		{
			Name:        "apple-light",
			Solarized:   appleLight,
			Accent1Pair: default1BgFg,
			Accent2Pair: default2BgFg,
		},
	}

	// The original solarized color palette
	solarized = cl.Solarized{
		Base: cl.Base{
			Base03: cl.SolarizedBase03,
			Base02: cl.SolarizedBase02,
			Base01: cl.SolarizedBase01,
			Base00: cl.SolarizedBase00,
			Base0:  cl.SolarizedBase0,
			Base1:  cl.SolarizedBase1,
			Base2:  cl.SolarizedBase2,
			Base3:  cl.SolarizedBase3,
		},
		Accents: cl.Accents{
			Blue:    cl.SolarizedBlue,
			Cyan:    cl.SolarizedCyan,
			Green:   cl.SolarizedGreen,
			Magenta: cl.SolarizedMagenta,
			Orange:  cl.SolarizedOrange,
			Red:     cl.SolarizedRed,
			Violet:  cl.SolarizedViolet,
			Yellow:  cl.SolarizedYellow,
		},
	}
	solarizedDarkHighContrast = cl.Solarized{
		Base:    solarized.Base.Clone().ChangeLightness(0.04, -0.02),
		Accents: solarized.Accents.Clone().ChangeLightness(0.05),
	}
	solarizedLightHighContrast = cl.Solarized{
		Base:    solarized.Base.Clone().ChangeLightness(0.02, -0.05),
		Accents: solarized.Accents.Clone().ChangeLightness(-0.05),
	}

	// the current -d colors in the emacs theme
	oldDarkAccents = cl.Accents{
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
	oldLightAccents = cl.Accents{
		Blue:    "#69B7F0",
		Cyan:    "#69CABF",
		Green:   "#B4C342",
		Magenta: "#F771AC",
		Orange:  "#F2804F",
		Red:     "#FF6E64",
		Violet:  "#9EA0E5",
		Yellow:  "#DEB542",
	}
	gruvboxDark = cl.Solarized{
		Base: cl.Base{
			Base03: cl.GruvboxDark0,
			Base02: cl.GruvboxDark0Soft,
			Base01: cl.GruvboxDark4,
			Base00: cl.GruvboxDark0,
			Base0:  cl.GruvboxLight4,
			Base1:  cl.GruvboxLight3,
			Base2:  cl.GruvboxLight4,
			Base3:  cl.GruvboxLight0,
		},
		Accents: cl.Accents{
			Blue:    cl.GruvboxBlue,
			Cyan:    cl.GruvboxAqua,
			Green:   cl.GruvboxGreen,
			Magenta: cl.GruvboxBrightPurple,
			Orange:  cl.GruvboxOrange,
			Red:     cl.GruvboxBrightRed,
			Violet:  cl.GruvboxPurple,
			Yellow:  cl.GruvboxYellow,
		},
	}
	// note: not inversed here
	gruvboxLight = cl.Solarized{
		Base: cl.Base{
			Base03: cl.GruvboxDark0,
			Base02: cl.GruvboxDark0Soft,
			Base01: cl.GruvboxDark3,
			Base00: cl.GruvboxDark4,
			Base0:  cl.GruvboxDark1,
			Base1:  cl.GruvboxLight4,
			Base2:  cl.GruvboxLight1,
			Base3:  cl.GruvboxLight0,
		},
		Accents: cl.Accents{
			Blue:    cl.GruvboxDarkBlue,
			Cyan:    cl.GruvboxAqua,
			Green:   cl.GruvboxGreen,
			Magenta: cl.GruvboxBrightPurple,
			Orange:  cl.GruvboxDarkOrange,
			Red:     cl.GruvboxDarkRed,
			Violet:  cl.GruvboxDarkPurple,
			Yellow:  cl.GruvboxDarkYellow,
		},
	}
	zenburn = cl.Solarized{
		Base: cl.Base{
			Base03: cl.ZenburnBg,
			Base02: cl.ZenburnBgP1,
			Base01: cl.HexColor(cl.ZenburnFgM1).Blend(cl.ZenburnFg, 0.3),
			Base00: cl.ZenburnBgP3,
			Base0:  cl.ZenburnFg,
			Base1:  cl.ZenburnFgP1,
			Base2:  cl.HexColor(cl.ZenburnFgP1).Blend(cl.ZenburnFgP2, 0.5),
			Base3:  cl.ZenburnFgP2,
		},
		Accents: cl.Accents{
			Blue:    cl.ZenburnBlue,
			Cyan:    cl.ZenburnCyan,
			Green:   cl.ZenburnGreen,
			Magenta: cl.ZenburnMagenta,
			Orange:  cl.ZenburnOrange,
			Red:     cl.ZenburnRed,
			Violet:  cl.HexColor(cl.ZenburnBlue).Blend(cl.ZenburnMagenta, 0.5),
			Yellow:  cl.ZenburnYellow,
		},
	}

	monokai = cl.Solarized{
		Base: cl.Base{
			Base03: cl.Monokai03,
			Base02: cl.Monokai02,
			Base01: cl.Monokai01,
			Base00: cl.Monokai00,
			Base0:  cl.Monokai0,
			Base1:  cl.Monokai0,
			Base2:  cl.Monokai0,
			Base3:  cl.Monokai0,
		},
		Accents: cl.Accents{
			Blue:    cl.MonokaiBlue,
			Cyan:    cl.MonokaiCyan,
			Green:   cl.MonokaiGreen,
			Magenta: cl.MonokaiMagenta,
			Orange:  cl.MonokaiOrange,
			Red:     cl.MonokaiRed,
			Violet:  cl.MonokaiViolet,
			Yellow:  cl.MonokaiYellow,
		},
	}
	selenizedDark = cl.Solarized{
		Base: cl.Base{
			Base03: "#103c48",
			Base02: "#184956",
			Base01: "#72898f",
			Base00: "#103c48",
			Base0:  "#adbcbc",
			Base1:  "#cad8d9",
			Base2:  "#ece3cc",
			Base3:  "#fbf3db",
		},
		Accents: cl.Accents{
			Blue:    "#4695f7",
			Cyan:    "#41c7b9",
			Green:   "#75b938",
			Magenta: "#f275be",
			Orange:  "#ed8649",
			Red:     "#fa5750",
			Violet:  "#af88eb",
			Yellow:  "#dbb32d",
		},
	}
	selenizedBlack = cl.Solarized{
		Base: cl.Base{
			Base03: "#181818",
			Base02: "#252525",
			Base01: "#777777",
			Base00: "#181818",
			Base0:  "#b9b9b9",
			Base1:  "#dedede",
			Base2:  "#53676d",
			Base3:  "#3a4d53",
		},
		Accents: cl.Accents{
			Blue:    "#368aeb",
			Cyan:    "#3fc5b7",
			Green:   "#70b433",
			Magenta: "#eb6eb7",
			Orange:  "#e67f43",
			Red:     "#ed4a46",
			Violet:  "#a580e2",
			Yellow:  "#dbb32d",
		},
	}
	selenizedLight = cl.Solarized{
		Base: cl.Base{
			Base03: "#fbf3db",
			Base02: "#ece3cc",
			Base01: "#909995",
			Base00: "#fbf3db",
			Base0:  "#53676d",
			Base1:  "#3a4d53",
			Base2:  "#adbcbc",
			Base3:  "#cad8d9",
		},
		Accents: cl.Accents{
			Blue:    "#0072d4",
			Cyan:    "#009c8f",
			Green:   "#489100",
			Magenta: "#ca4898",
			Orange:  "#c25d1e",
			Red:     "#d2212d",
			Violet:  "#8762c6",
			Yellow:  "#ad8900",
		},
	}
	selenizedWhite = cl.Solarized{
		Base: cl.Base{
			Base03: "#ffffff",
			Base02: "#ebebeb",
			Base01: "#878787",
			Base00: "#ffffff",
			Base0:  "#474747",
			Base1:  "#282828",
			Base2:  "#b9b9b9",
			Base3:  "#dedede",
		},
		Accents: cl.Accents{
			Blue:    "#0064e4",
			Cyan:    "#00ad9c",
			Green:   "#1d9700",
			Magenta: "#dd0f9d",
			Orange:  "#d04a00",
			Red:     "#d6000c",
			Violet:  "#7f51d6",
			Yellow:  "#c49700",
		},
	}
	// Everforest dark, medium contrast (sainnhe/everforest). Base00, which some faces use as
	// text, is the comment color rather than the background. Everforest has one
	// purple, Magenta; Violet is that purple turned toward blue (OKLCH hue 300, same
	// lightness and chroma).
	everforestDark = cl.Solarized{
		Base: cl.Base{
			Base03: "#2d353b",
			Base02: "#343f44",
			Base01: "#859289",
			Base00: "#859289",
			Base0:  "#d3c6aa",
			Base1:  "#d3c6aa",
			Base2:  "#f4f0d9",
			Base3:  "#fdf6e3",
		},
		Accents: cl.Accents{
			Blue:    "#7fbbb3",
			Cyan:    "#83c092",
			Green:   "#a7c080",
			Magenta: "#d699b6",
			Orange:  "#e69875",
			Red:     "#e67e80",
			Violet:  "#b6a3db",
			Yellow:  "#dbbc7f",
		},
	}

	// Everforest light, medium contrast (sainnhe/everforest). Base00, which some faces use as
	// text, is the comment color rather than the background. Everforest has one
	// purple, Magenta; Violet is that purple turned toward blue (OKLCH hue 300).
	everforestLight = cl.Solarized{
		Base: cl.Base{
			Base03: "#fdf6e3",
			Base02: "#f4f0d9",
			Base01: "#939f91",
			Base00: "#939f91",
			Base0:  "#5c6a72",
			Base1:  "#5c6a72",
			Base2:  "#d3c6aa",
			Base3:  "#d3c6aa",
		},
		Accents: cl.Accents{
			Blue:    "#3a94c5",
			Cyan:    "#35a77c",
			Green:   "#8da101",
			Magenta: "#df69ba",
			Orange:  "#f57d26",
			Red:     "#f85552",
			Violet:  "#9d79d7",
			Yellow:  "#dfa000",
		},
	}

	// Flexoki dark (stephango.com/flexoki): bg, bg-2, tx-3, tx and the 400 accents. Base00, which some faces use as
	// text, is the comment color rather than the background.
	flexokiDark = cl.Solarized{
		Base: cl.Base{
			Base03: "#100f0f",
			Base02: "#1c1b1a",
			Base01: "#575653",
			Base00: "#575653",
			Base0:  "#cecdc3",
			Base1:  "#cecdc3",
			Base2:  "#f2f0e5",
			Base3:  "#fffcf0",
		},
		Accents: cl.Accents{
			Blue:    "#4385be",
			Cyan:    "#3aa99f",
			Green:   "#879a39",
			Magenta: "#ce5d97",
			Orange:  "#da702c",
			Red:     "#d14d41",
			Violet:  "#8b7ec8",
			Yellow:  "#d0a215",
		},
	}

	// Flexoki light (stephango.com/flexoki): paper, bg-2, tx-3, tx and the 600 accents. Base00, which some faces use as
	// text, is the comment color rather than the background.
	flexokiLight = cl.Solarized{
		Base: cl.Base{
			Base03: "#fffcf0",
			Base02: "#f2f0e5",
			Base01: "#b7b5ac",
			Base00: "#b7b5ac",
			Base0:  "#100f0f",
			Base1:  "#100f0f",
			Base2:  "#cecdc3",
			Base3:  "#cecdc3",
		},
		Accents: cl.Accents{
			Blue:    "#205ea6",
			Cyan:    "#24837b",
			Green:   "#66800b",
			Magenta: "#a02f6f",
			Orange:  "#bc5215",
			Red:     "#af3029",
			Violet:  "#5e409d",
			Yellow:  "#ad8301",
		},
	}

	// Kanagawa wave (rebelot/kanagawa.nvim). Base00, which some faces use as
	// text, is the comment color rather than the background.
	kanagawaWave = cl.Solarized{
		Base: cl.Base{
			Base03: "#1f1f28",
			Base02: "#2a2a37",
			Base01: "#727169",
			Base00: "#727169",
			Base0:  "#dcd7ba",
			Base1:  "#dcd7ba",
			Base2:  "#e7dba0",
			Base3:  "#f2ecbc",
		},
		Accents: cl.Accents{
			Blue:    "#7e9cd8",
			Cyan:    "#6a9589",
			Green:   "#76946a",
			Magenta: "#d27e99",
			Orange:  "#ffa066",
			Red:     "#c34043",
			Violet:  "#957fb8",
			Yellow:  "#c0a36e",
		},
	}

	// Kanagawa dragon (rebelot/kanagawa.nvim). Base00, which some faces use as
	// text, is the comment color rather than the background. Its terminal blue and
	// cyan (dragonBlue2, dragonAqua) are nearly the same color, so Blue is
	// dragonBlue and Cyan is waveAqua2. Changes:
	//   - Orange: dragonOrange (#b6927b) with OKLCH chroma raised from 0.054 to
	//     0.09, same hue and lightness, as it read as a tan between red and yellow.
	kanagawaDragon = cl.Solarized{
		Base: cl.Base{
			Base03: "#181616",
			Base02: "#282727",
			Base01: "#737c73",
			Base00: "#737c73",
			Base0:  "#c5c9c5",
			Base1:  "#c5c9c5",
			Base2:  "#e7dba0",
			Base3:  "#f2ecbc",
		},
		Accents: cl.Accents{
			Blue:    "#658594",
			Cyan:    "#7aa89f",
			Green:   "#8a9a7b",
			Magenta: "#a292a3",
			Orange:  "#c68b64",
			Red:     "#c4746e",
			Violet:  "#8992a7",
			Yellow:  "#c4b28a",
		},
	}

	// Kanagawa lotus (rebelot/kanagawa.nvim). Base00, which some faces use as
	// text, is the comment color rather than the background. Violet is lotusBlue5,
	// as lotusViolet4 is the terminal's bright magenta.
	kanagawaLotus = cl.Solarized{
		Base: cl.Base{
			Base03: "#f2ecbc",
			Base02: "#e7dba0",
			Base01: "#8a8980",
			Base00: "#8a8980",
			Base0:  "#545464",
			Base1:  "#43436c",
			Base2:  "#dcd7ba",
			Base3:  "#dcd7ba",
		},
		Accents: cl.Accents{
			Blue:    "#4d699b",
			Cyan:    "#597b75",
			Green:   "#6f894e",
			Magenta: "#b35b79",
			Orange:  "#cc6d00",
			Red:     "#c84053",
			Violet:  "#5d57a3",
			Yellow:  "#77713f",
		},
	}

	// Nightfox palettes (EdenEast/nightfox.nvim): Base03 is bg1, Base02 bg2 (bg0
	// in the light variants), Base01 and Base00 the comment color, Base0 fg1 and
	// Base1 fg0. Its purple ("magenta") is Violet and its pink Magenta.

	// Nightfox. Base2 and Base3 are dayfox's backgrounds.
	nightfox = cl.Solarized{
		Base: cl.Base{
			Base03: "#192330",
			Base02: "#212e3f",
			Base01: "#738091",
			Base00: "#738091",
			Base0:  "#cdcecf",
			Base1:  "#d6d6d7",
			Base2:  "#e4dcd4",
			Base3:  "#f6f2ee",
		},
		Accents: cl.Accents{
			Blue:    "#719cd6",
			Cyan:    "#63cdcf",
			Green:   "#81b29a",
			Magenta: "#d67ad2",
			Orange:  "#f4a261",
			Red:     "#c94f6d",
			Violet:  "#9d79d6",
			Yellow:  "#dbc074",
		},
	}

	// Dayfox.
	dayfox = cl.Solarized{
		Base: cl.Base{
			Base03: "#f6f2ee",
			Base02: "#e4dcd4",
			Base01: "#837a72",
			Base00: "#837a72",
			Base0:  "#3d2b5a",
			Base1:  "#302b5d",
			Base2:  "#cdcecf",
			Base3:  "#cdcecf",
		},
		Accents: cl.Accents{
			Blue:    "#2848a9",
			Cyan:    "#287980",
			Green:   "#396847",
			Magenta: "#a440b5",
			Orange:  "#955f61",
			Red:     "#a5222f",
			Violet:  "#6e33ce",
			Yellow:  "#ac5402",
		},
	}

	// Dawnfox. Changes:
	//   - Yellow: #ea9d34 toned down to OKLCH lightness 0.64, chroma 0.11 (same
	//     hue), in line with the other accents.
	dawnfox = cl.Solarized{
		Base: cl.Base{
			Base03: "#faf4ed",
			Base02: "#ebe5df",
			Base01: "#9893a5",
			Base00: "#9893a5",
			Base0:  "#575279",
			Base1:  "#4c4769",
			Base2:  "#e0def4",
			Base3:  "#e0def4",
		},
		Accents: cl.Accents{
			Blue:    "#286983",
			Cyan:    "#56949f",
			Green:   "#618774",
			Magenta: "#d685af",
			Orange:  "#d7827e",
			Red:     "#b4637a",
			Violet:  "#907aa9",
			Yellow:  "#b77f39",
		},
	}

	// Duskfox. Base2 and Base3 are dawnfox's backgrounds.
	duskfox = cl.Solarized{
		Base: cl.Base{
			Base03: "#232136",
			Base02: "#2d2a45",
			Base01: "#817c9c",
			Base00: "#817c9c",
			Base0:  "#e0def4",
			Base1:  "#eae8ff",
			Base2:  "#ebe5df",
			Base3:  "#faf4ed",
		},
		Accents: cl.Accents{
			Blue:    "#569fba",
			Cyan:    "#9ccfd8",
			Green:   "#a3be8c",
			Magenta: "#eb98c3",
			Orange:  "#ea9a97",
			Red:     "#eb6f92",
			Violet:  "#c4a7e7",
			Yellow:  "#f6c177",
		},
	}

	// Nordfox. Base2 and Base3 are its white and bright white. Changes:
	//   - Base1: its white, as fg0 has less contrast than fg1.
	//   - Violet: its purple turned to OKLCH hue 300, as Magenta (its pink) has
	//     nearly the same hue.
	nordfox = cl.Solarized{
		Base: cl.Base{
			Base03: "#2e3440",
			Base02: "#39404f",
			Base01: "#60728a",
			Base00: "#60728a",
			Base0:  "#cdcecf",
			Base1:  "#e5e9f0",
			Base2:  "#e5e9f0",
			Base3:  "#e7ecf4",
		},
		Accents: cl.Accents{
			Blue:    "#81a1c1",
			Cyan:    "#88c0d0",
			Green:   "#a3be8c",
			Magenta: "#bf88bc",
			Orange:  "#c9826b",
			Red:     "#bf616a",
			Violet:  "#a294bd",
			Yellow:  "#ebcb8b",
		},
	}

	// Terafox. Base2 and Base3 are its white and bright white. Changes:
	//   - Violet: its magenta turned to OKLCH hue 300; it has no purple.
	terafox = cl.Solarized{
		Base: cl.Base{
			Base03: "#152528",
			Base02: "#1d3337",
			Base01: "#6d7f8b",
			Base00: "#6d7f8b",
			Base0:  "#e6eaea",
			Base1:  "#eaeeee",
			Base2:  "#ebebeb",
			Base3:  "#eeeeee",
		},
		Accents: cl.Accents{
			Blue:    "#5a93aa",
			Cyan:    "#a1cdd8",
			Green:   "#7aa4a1",
			Magenta: "#ad5c7c",
			Orange:  "#ff8349",
			Red:     "#e85c51",
			Violet:  "#846ab2",
			Yellow:  "#fda47f",
		},
	}

	// Carbonfox. Base2 and Base3 are its white and bright white; bg and fg are
	// the values upstream computes. Changes:
	//   - Yellow, Orange, Red: IBM Carbon's yellow 30, orange 40 and red 50;
	//     upstream's are two teals and a magenta.
	//   - Cyan: its teal (upstream's yellow), as its cyan is close to its blue.
	carbonfox = cl.Solarized{
		Base: cl.Base{
			Base03: "#161616",
			Base02: "#252525",
			Base01: "#6e6f70",
			Base00: "#6e6f70",
			Base0:  "#f2f4f8",
			Base1:  "#f9fbff",
			Base2:  "#dfdfe0",
			Base3:  "#e4e4e5",
		},
		Accents: cl.Accents{
			Blue:    "#78a9ff",
			Cyan:    "#08bdba",
			Green:   "#25be6a",
			Magenta: "#ff7eb6",
			Orange:  "#ff832b",
			Red:     "#fa4d56",
			Violet:  "#be95ff",
			Yellow:  "#f1c21b",
		},
	}

	// IBM Carbon (carbon-design-system/carbon), from its g100 and white themes:
	// Base03 is background, Base02 layer-01, Base0 text-secondary and Base1
	// text-primary. Each accent family is taken at one grade, which Carbon
	// keeps at equal contrast across families; Cyan is teal, as Carbon's cyan
	// is close to its blue, and Violet is purple.

	// Carbon dark (g100): grade 40 accents, gray 60 comments. Base2 and Base3
	// are the white theme's backgrounds.
	carbonDark = cl.Solarized{
		Base: cl.Base{
			Base03: "#161616",
			Base02: "#262626",
			Base01: "#6f6f6f",
			Base00: "#6f6f6f",
			Base0:  "#c6c6c6",
			Base1:  "#f4f4f4",
			Base2:  "#f4f4f4",
			Base3:  "#ffffff",
		},
		Accents: cl.Accents{
			Blue:    "#78a9ff",
			Cyan:    "#08bdba",
			Green:   "#42be65",
			Magenta: "#ff7eb6",
			Orange:  "#ff832b",
			Red:     "#ff8389",
			Violet:  "#be95ff",
			Yellow:  "#d2a106",
		},
	}

	// Carbon light (white): grade 60 accents, gray 50 comments. Changes:
	//   - Blue: cyan 60, as blue 60 is far more saturated than the others.
	carbonLight = cl.Solarized{
		Base: cl.Base{
			Base03: "#ffffff",
			Base02: "#f4f4f4",
			Base01: "#8d8d8d",
			Base00: "#8d8d8d",
			Base0:  "#525252",
			Base1:  "#161616",
			Base2:  "#c6c6c6",
			Base3:  "#c6c6c6",
		},
		Accents: cl.Accents{
			Blue:    "#0072c3",
			Cyan:    "#007d79",
			Green:   "#198038",
			Magenta: "#d02670",
			Orange:  "#ba4e00",
			Red:     "#da1e28",
			Violet:  "#8a3ffc",
			Yellow:  "#8e6a00",
		},
	}

	// Apple Human Interface Guidelines colors (June 2025 values): Base03 is
	// gray 6 (white in light), Base02 gray 5 (gray 6), Base01 and Base00 system
	// gray, Base0 the secondary label color (without its opacity) and Base1 the
	// primary label. Magenta is purple, Violet indigo and Cyan mint, as pink is
	// close to red and teal and cyan to blue.

	// Apple dark: system colors. Base2 and Base3 are the light backgrounds.
	appleDark = cl.Solarized{
		Base: cl.Base{
			Base03: "#1c1c1e",
			Base02: "#2c2c2e",
			Base01: "#8e8e93",
			Base00: "#8e8e93",
			Base0:  "#ebebf5",
			Base1:  "#ffffff",
			Base2:  "#f2f2f7",
			Base3:  "#ffffff",
		},
		Accents: cl.Accents{
			Blue:    "#0091ff",
			Cyan:    "#00dac3",
			Green:   "#30d158",
			Magenta: "#db34f2",
			Orange:  "#ff9230",
			Red:     "#ff4245",
			Violet:  "#6d7cff",
			Yellow:  "#ffd600",
		},
	}

	// Apple light: the increased contrast colors, as the system yellow, orange,
	// green and mint are under 2.4:1 on white.
	appleLight = cl.Solarized{
		Base: cl.Base{
			Base03: "#ffffff",
			Base02: "#f2f2f7",
			Base01: "#8e8e93",
			Base00: "#8e8e93",
			Base0:  "#3c3c43",
			Base1:  "#000000",
			Base2:  "#ebebf5",
			Base3:  "#ebebf5",
		},
		Accents: cl.Accents{
			Blue:    "#1e6ef4",
			Cyan:    "#008575",
			Green:   "#008932",
			Magenta: "#b02fc2",
			Orange:  "#c55300",
			Red:     "#e9152d",
			Violet:  "#564ade",
			Yellow:  "#a16a00",
		},
	}
)

// PrintAlist prints the named colors list in a way that is compatible with solarized-palettes.el.
func PrintAlist(w io.Writer, n cl.NamedColors, indent int) (int, error) {
	keys := cl.OrderedKeys(n)
	var longestKey int
	for _, n := range keys {
		l := len(n)
		if l > longestKey {
			longestKey = l
		}
	}
	var prefix strings.Builder
	for i := 0; i < indent; i++ {
		prefix.WriteString(" ")

	}
	var nt int

	for _, name := range keys {
		n, err := fmt.Fprintf(w, "%s(%-"+strconv.Itoa(longestKey)+"s . \"%s\")\n", prefix.String(), name, n[name])
		n = n + nt
		if err != nil {
			return nt, err
		}

	}
	return nt, nil
}

func RewritePalette(nc cl.NamedColors, paletteName string) {

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
			PrintAlist(&dst, nc, 4)
		}
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	file.Close()
	ioutil.WriteFile("../solarized-palettes.el", dst.Bytes(), 0x776)

}
