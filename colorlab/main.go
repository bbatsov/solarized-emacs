package main

import (
	"bufio"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"math"
	"os"
	"sort"
	"strconv"
	"strings"

	"bytes"

	"github.com/bbatsov/solarized-emacs/colorlab/pkg/colors"
	colorful "github.com/lucasb-eyer/go-colorful"
)

var palettes = []Palette{
	{
		Name:      "solarized-dark",
		Solarized: solarized,
	},
	{
		Name:      "solarized-light",
		Solarized: solarized,
		Inverse:   true,
	},
	{
		Name:      "solarized-dark-high-contrast",
		Solarized: solarizedDarkHighContrast,
	},
	{
		Name:      "solarized-light-high-contrast",
		Solarized: solarizedLightHighContrast,
		Inverse:   true,
	},
	{
		Name:      "gruvbox-dark",
		Solarized: gruvboxDark,
	},
	{
		Name:      "gruvbox-light",
		Solarized: gruvboxLight,
		Inverse:   true,
	},
	{
		Name:      "zenburn",
		Solarized: zenburn,
	},
	{
		Name:      "monokai",
		Solarized: monokai,
	},
}

func main() {

	for _, pal := range palettes {
		fmt.Println(pal.Name)
		nc := pal.Generate()

		nc.PrintAlist(os.Stdout, 0)
		// nc.FilterPrefix("base").PrintAlist(os.Stdout, 0)
		// Merge(nc.FilterSuffix("-d"), oldDarkAccents.NamedColors().WithSuffix("-do")).PrintAlist(os.Stdout, 0)
		// Merge(nc.FilterSuffix("-l"), oldLightAccents.NamedColors().WithSuffix("-lo")).PrintAlist(os.Stdout, 0)

		rewriteTheme(nc, pal.Name)
	}
	fmt.Println("\n-----\n")
}

func (p Palette) Generate() NamedColors {

	pal := p.Solarized

	corrected := pal
	if p.Inverse {
		corrected = pal.Inverse()
	}

	bgs, fgs := createComplementaryColors(corrected, 0.85, 0.3, 0.01)
	hbgs, hfgs := createComplementaryColors(corrected, 0.6, 0.45, 0.04)
	cols := Merge(
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

func createComplementaryColors(sol Solarized, blendBg, blendFg, gamma float64) (accentBgs, accentFgs Accents) {
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

		blendBgColor := sol.Base03.Color()
		blendFgColor := sol.Base0.Color()
		brightOnDark := true
		if light(blendFgColor) < light(blendBgColor) {
			brightOnDark = false
		}
		for _, v := range sol.Base.Colors() {
			if brightOnDark {
				if light(v) > light(blendFgColor) {
					blendFgColor = v
				}
				if light(v) < light(blendBgColor) {
					blendBgColor = v
				}
			} else {
				if light(v) < light(blendFgColor) {
					blendFgColor = v
				}
				if light(v) > light(blendBgColor) {
					blendBgColor = v
				}
			}
		}

		bg := v.BlendLab(blendBgColor, blendBg)
		fg := v.BlendLab(blendFgColor, blendFg)
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
			nlb = nlb - gamma
			nlf = nlf - gamma
		} else {
			nlb = nlb + gamma
			nlf = nlf + gamma
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
	return NewAccents(bgs), NewAccents(fgs)
}

type Solarized struct {
	Base
	Accents
	// AccentsBg Accents
	// AccentsFg Accents
}

func (s Solarized) Clone() Solarized {
	return Solarized{
		Base:    s.Base.Clone(),
		Accents: s.Accents.Clone(),
		// AccentsFg: s.AccentsFg,
		// AccentsBg: s.AccentsBg,
	}
}

func (s Solarized) Inverse() Solarized {
	return Solarized{
		Base:    s.Base.Inverse(),
		Accents: s.Accents.Clone(),
		// AccentsFg: s.AccentsFg,
		// AccentsBg: s.AccentsBg,
	}
}
func (s Solarized) NamedColors() NamedColors {
	return Merge(s.Base.NamedColors(), s.Accents.NamedColors())

}

type HexColor string

func (h HexColor) Color() colorful.Color {
	col, err := colorful.Hex(string(h))
	if err != nil {
		panic(err)
	}
	return col
}

func (h HexColor) Blend(hc HexColor, t float64) HexColor {
	c1 := h.Color()
	c2 := hc.Color()
	return HexColor(c1.BlendLab(c2, t).Hex())
}

type Base struct {
	Base03 HexColor
	Base02 HexColor
	Base01 HexColor
	Base00 HexColor
	Base0  HexColor
	Base1  HexColor
	Base2  HexColor
	Base3  HexColor
}

func NewBase(bc [8]colorful.Color) Base {
	return Base{
		Base03: HexColor(bc[0].Clamped().Hex()),
		Base02: HexColor(bc[1].Clamped().Hex()),
		Base01: HexColor(bc[2].Clamped().Hex()),
		Base00: HexColor(bc[3].Clamped().Hex()),
		Base0:  HexColor(bc[4].Clamped().Hex()),
		Base1:  HexColor(bc[5].Clamped().Hex()),
		Base2:  HexColor(bc[6].Clamped().Hex()),
		Base3:  HexColor(bc[7].Clamped().Hex()),
	}
}
func (s Base) Clone() Base {
	return Base{
		Base03: s.Base03,
		Base02: s.Base02,
		Base01: s.Base01,
		Base00: s.Base00,
		Base0:  s.Base0,
		Base1:  s.Base1,
		Base2:  s.Base2,
		Base3:  s.Base3,
	}
}
func (s Base) NamedColors() NamedColors {
	nc := make(NamedColors, 8)
	arr := s.colorArray()
	for idx, hex := range arr {
		nc[baseNames[idx]] = hex
	}
	return nc
}

func (s Base) Colors() BaseColors {
	a := s.colorArray()
	return [8]colorful.Color{
		a[0].Color(),
		a[1].Color(),
		a[2].Color(),
		a[3].Color(),
		a[4].Color(),
		a[5].Color(),
		a[6].Color(),
		a[7].Color(),
	}
}
func (s Base) colorArray() [8]HexColor {
	return [8]HexColor{
		s.Base03,
		s.Base02,
		s.Base01,
		s.Base00,
		s.Base0,
		s.Base1,
		s.Base2,
		s.Base3,
	}
}

var baseNames = [8]string{
	0: "base03",
	1: "base02",
	2: "base01",
	3: "base00",
	4: "base0",
	5: "base1",
	6: "base2",
	7: "base3",
}

type BaseColors [8]colorful.Color

func (s Base) Inverse() Base {
	return Base{
		Base03: s.Base3,
		Base02: s.Base2,
		Base01: s.Base1,
		Base00: s.Base0,
		Base0:  s.Base00,
		Base1:  s.Base01,
		Base2:  s.Base02,
		Base3:  s.Base03,
	}
}
func (b Base) ChangeLightness(amountFgs, amountBgs float64) Base {
	cc := b.Colors()
	lightness := func(c colorful.Color, v float64) colorful.Color {
		l, a, b := c.Lab()
		l = l + v
		return colorful.Lab(l, a, b)
	}
	cc[0] = lightness(cc[0], amountBgs)
	cc[1] = lightness(cc[1], amountBgs)
	cc[2] = lightness(cc[2], amountFgs)
	cc[3] = lightness(cc[3], amountBgs)
	cc[4] = lightness(cc[4], amountFgs)
	cc[5] = lightness(cc[5], amountFgs)
	cc[6] = lightness(cc[6], amountFgs)
	cc[7] = lightness(cc[7], amountFgs)
	return NewBase(cc)
}

// Accents .
type Accents struct {
	Yellow  HexColor
	Orange  HexColor
	Red     HexColor
	Magenta HexColor
	Violet  HexColor
	Blue    HexColor
	Cyan    HexColor
	Green   HexColor
}

func NewAccents(cc [8]colorful.Color) Accents {
	return Accents{
		Yellow:  HexColor(cc[0].Clamped().Hex()),
		Orange:  HexColor(cc[1].Clamped().Hex()),
		Red:     HexColor(cc[2].Clamped().Hex()),
		Magenta: HexColor(cc[3].Clamped().Hex()),
		Violet:  HexColor(cc[4].Clamped().Hex()),
		Blue:    HexColor(cc[5].Clamped().Hex()),
		Cyan:    HexColor(cc[6].Clamped().Hex()),
		Green:   HexColor(cc[7].Clamped().Hex()),
	}
}
func (s Accents) Clone() Accents {
	return Accents{
		Yellow:  s.Yellow,
		Orange:  s.Orange,
		Red:     s.Red,
		Magenta: s.Magenta,
		Violet:  s.Violet,
		Blue:    s.Blue,
		Cyan:    s.Cyan,
		Green:   s.Green,
	}
}

func (s Accents) Colors() AccentColors {
	a := s.colorArray()
	return [8]colorful.Color{
		a[0].Color(),
		a[1].Color(),
		a[2].Color(),
		a[3].Color(),
		a[4].Color(),
		a[5].Color(),
		a[6].Color(),
		a[7].Color(),
	}
}
func (a Accents) ChangeLightness(amount float64) Accents {
	cc := a.Colors()
	lightness := func(c colorful.Color, v float64) colorful.Color {
		l, a, b := c.Lab()
		l = l + v
		return colorful.Lab(l, a, b)
	}
	cc[0] = lightness(cc[0], amount)
	cc[1] = lightness(cc[1], amount)
	cc[2] = lightness(cc[2], amount)
	cc[3] = lightness(cc[3], amount)
	cc[4] = lightness(cc[4], amount)
	cc[5] = lightness(cc[5], amount)
	cc[6] = lightness(cc[6], amount)
	cc[7] = lightness(cc[7], amount)
	return NewAccents(cc)
}
func (a Accents) ChangeSaturation(amount float64) Accents {
	cc := a.Colors()
	saturation := func(c colorful.Color, v float64) colorful.Color {
		h, s, l := c.Hsl()
		// s = math.Min(math.Max(s+v, 0), 1)
		s = s + v
		return colorful.Hsl(h, s, l)
	}
	cc[0] = saturation(cc[0], amount)
	cc[1] = saturation(cc[1], amount)
	cc[2] = saturation(cc[2], amount)
	cc[3] = saturation(cc[3], amount)
	cc[4] = saturation(cc[4], amount)
	cc[5] = saturation(cc[5], amount)
	cc[6] = saturation(cc[6], amount)
	cc[7] = saturation(cc[7], amount)
	return NewAccents(cc)
}

func (a Accents) Blend(hc HexColor, amount float64) Accents {
	c := hc.Color()
	cc := a.Colors()
	cc[0] = cc[0].BlendLab(c, amount)
	cc[1] = cc[1].BlendLab(c, amount)
	cc[2] = cc[2].BlendLab(c, amount)
	cc[3] = cc[3].BlendLab(c, amount)
	cc[4] = cc[4].BlendLab(c, amount)
	cc[5] = cc[5].BlendLab(c, amount)
	cc[6] = cc[6].BlendLab(c, amount)
	cc[7] = cc[7].BlendLab(c, amount)
	return NewAccents(cc)
}

func (ac Accents) NamedColors() NamedColors {
	nc := make(NamedColors, 8)
	arr := ac.colorArray()
	for idx, hex := range arr {
		nc[accentNames[idx]] = hex
	}
	return nc
}

func (s Accents) colorArray() [8]HexColor {
	return [8]HexColor{
		s.Yellow,
		s.Orange,
		s.Red,
		s.Magenta,
		s.Violet,
		s.Blue,
		s.Cyan,
		s.Green,
	}
}

var accentNames = [8]string{
	0: "yellow",
	1: "orange",
	2: "red",
	3: "magenta",
	4: "violet",
	5: "blue",
	6: "cyan",
	7: "green",
}

type AccentColors [8]colorful.Color

type NamedColors map[string]HexColor

func (n NamedColors) WithPrefix(prefix string) NamedColors {
	nc := make(NamedColors, len(n))
	for k, v := range n {
		nc[fmt.Sprintf("%s%s", prefix, k)] = v
	}
	return nc
}

func (n NamedColors) WithSuffix(suffix string) NamedColors {
	nc := make(NamedColors, len(n))
	for k, v := range n {
		nc[fmt.Sprintf("%s%s", k, suffix)] = v
	}
	return nc
}

func (n NamedColors) FilterSuffix(suffix ...string) NamedColors {
	nc := make(NamedColors, len(n))
	for k, v := range n {
	loop:
		for _, s := range suffix {
			if strings.HasSuffix(k, s) {
				nc[k] = v
				break loop
			}
		}
	}
	return nc
}

func (n NamedColors) FilterPrefix(prefix ...string) NamedColors {
	nc := make(NamedColors, len(n))
	for k, v := range n {
	loop:
		for _, p := range prefix {
			if strings.HasPrefix(k, p) {
				nc[k] = v
				break loop
			}
		}
	}
	return nc
}

func (n NamedColors) PrintAlist(w io.Writer, indent int) (int, error) {
	keys := n.OrderedKeys()
	var longestKey int
	for _, n := range keys {
		l := len(n)
		if l > longestKey {
			longestKey = l
		}
	}
	longestKey = longestKey
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

// Orderedkeys returns the keys in a order that is like how solarized names should be ordered.
//
// primary orer is:
//
// 1. base names
// 2. accent colors
// 3. names that contain base names
// 4. names that contain accent color names
// 5. the rest
//
// secondary sort order is by string comparison.
//
func (n NamedColors) OrderedKeys() []string {
	var names []string
	for k, _ := range n {
		names = append(names, k)
	}
	sort.Sort(sort.StringSlice(names))

	var allNames []string
	for _, n := range baseNames {
		allNames = append(allNames, n)
	}
	for _, n := range accentNames {
		allNames = append(allNames, n)
	}
	score := func(idx int, exact bool) int {
		s := (len(allNames) - idx) + 1
		if exact {
			s = s + len(allNames)
		}
		return s
	}
	sort.SliceStable(names, func(i int, j int) bool {
		var is, js int // scores
		in := names[i]
		jn := names[j]
	scoring:
		for sortIdx, sortName := range allNames {
			if is == 0 {
				// log.Println(in, sortName)
				if strings.Contains(in, sortName) {
					is = score(sortIdx, in == sortName)
					// fmt.Println("score in", in, sortName, is)
				}
			}
			if js == 0 {
				if strings.Contains(jn, sortName) {
					js = score(sortIdx, jn == sortName)
					// fmt.Println("score j", jn, sortName, js)
				}
			}
			if is != 0 && js != 0 {
				break scoring
			}
		}
		// log.Println(is, js, in,jn)
		return is > js
	})
	return names
}

func Merge(nc ...NamedColors) NamedColors {
	res := make(NamedColors)
	for _, nc := range nc {
		for k, v := range nc {
			res[k] = v
		}
	}
	return res
}

var ( // The original solarized color palette
	solarized = Solarized{
		Base: Base{
			Base03: colors.SolarizedBase03,
			Base02: colors.SolarizedBase02,
			Base01: colors.SolarizedBase01,
			Base00: colors.SolarizedBase00,
			Base0:  colors.SolarizedBase0,
			Base1:  colors.SolarizedBase1,
			Base2:  colors.SolarizedBase2,
			Base3:  colors.SolarizedBase3,
		},
		Accents: Accents{
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
	solarizedDarkHighContrast = Solarized{
		Base:    solarized.Base.Clone().ChangeLightness(0.04, -0.02),
		Accents: solarized.Accents.Clone().ChangeLightness(0.05),
	}
	solarizedLightHighContrast = Solarized{
		Base:    solarized.Base.Clone().ChangeLightness(0.02, -0.05),
		Accents: solarized.Accents.Clone().ChangeLightness(-0.05),
	}

	// the current -d colors in the emacs theme
	oldDarkAccents = Accents{
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
	oldLightAccents = Accents{
		Blue:    "#69B7F0",
		Cyan:    "#69CABF",
		Green:   "#B4C342",
		Magenta: "#F771AC",
		Orange:  "#F2804F",
		Red:     "#FF6E64",
		Violet:  "#9EA0E5",
		Yellow:  "#DEB542",
	}
	gruvboxDark = Solarized{
		Base: Base{
			Base03: colors.GruvboxDark0,
			Base02: colors.GruvboxDark0Soft,
			Base01: colors.GruvboxDark4,
			Base00: colors.GruvboxDark0,
			Base0:  colors.GruvboxLight4,
			Base1:  colors.GruvboxLight3,
			Base2:  colors.GruvboxLight4,
			Base3:  colors.GruvboxLight0,
		},
		Accents: Accents{
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
	gruvboxLight = Solarized{
		Base: Base{
			Base03: colors.GruvboxDark0,
			Base02: colors.GruvboxDark0Soft,
			Base01: colors.GruvboxDark3,
			Base00: colors.GruvboxDark4,
			Base0:  colors.GruvboxDark1,
			Base1:  colors.GruvboxLight4,
			Base2:  colors.GruvboxLight1,
			Base3:  colors.GruvboxLight0,
		},
		Accents: Accents{
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
	zenburn = Solarized{
		Base: Base{
			Base03: colors.ZenburnBg,
			Base02: colors.ZenburnBgP1,
			Base01: HexColor(colors.ZenburnFgM1).Blend(colors.ZenburnFg, 0.3),
			Base00: colors.ZenburnBgP3,
			Base0:  colors.ZenburnFg,
			Base1:  colors.ZenburnFgP1,
			Base2:  HexColor(colors.ZenburnFgP1).Blend(colors.ZenburnFgP2, 0.5),
			Base3:  colors.ZenburnFgP2,
		},
		Accents: Accents{
			Blue:    colors.ZenburnBlue,
			Cyan:    colors.ZenburnCyan,
			Green:   colors.ZenburnGreen,
			Magenta: colors.ZenburnMagenta,
			Orange:  colors.ZenburnOrange,
			Red:     colors.ZenburnRed,
			Violet:  HexColor(colors.ZenburnBlue).Blend(colors.ZenburnMagenta, 0.5),
			Yellow:  colors.ZenburnYellow,
		},
	}

	monokai = Solarized{
		Base: Base{
			Base03: colors.Monokai03,
			Base02: colors.Monokai02,
			Base01: colors.Monokai01,
			Base00: colors.Monokai00,
			Base0:  colors.Monokai0,
			Base1:  colors.Monokai0,
			Base2:  colors.Monokai0,
			Base3:  colors.Monokai0,
		},
		Accents: Accents{
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
	Solarized Solarized
}

func rewriteTheme(nc NamedColors, paletteName string) {

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
