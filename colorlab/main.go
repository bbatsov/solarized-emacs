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
	prefix := ""
	for i := 0; i < indent; i++ {
		prefix += " "

	}
	var nt int

	for _, name := range keys {
		n, err := fmt.Fprintf(w, "%s(%-"+strconv.Itoa(longestKey)+"s . \"%s\")\n", prefix, name, n[name])
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
