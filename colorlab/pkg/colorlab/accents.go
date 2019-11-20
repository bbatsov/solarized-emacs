package colorlab

import "github.com/lucasb-eyer/go-colorful"

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
