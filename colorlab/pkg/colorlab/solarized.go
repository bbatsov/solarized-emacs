package colorlab

import (
	"github.com/lucasb-eyer/go-colorful"
)

type Solarized struct {
	Base
	Accents
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

type BaseColors [8]colorful.Color

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

type AccentColors [8]colorful.Color

func (s Solarized) Clone() Solarized {
	return Solarized{
		Base:    s.Base.Clone(),
		Accents: s.Accents.Clone(),
	}
}

func (s Solarized) Inverse() Solarized {
	return Solarized{
		Base:    s.Base.Inverse(),
		Accents: s.Accents.Clone(),
	}
}

func (s Solarized) NamedColors() NamedColors {
	return Merge(s.Base.NamedColors(), s.Accents.NamedColors())

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
