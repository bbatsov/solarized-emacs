package colorlab

import (
	"log"

	"github.com/lucasb-eyer/go-colorful"
)

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

// Returns true if the LAB lightness of base0 is larger than base03.
func (b Base) IsDarkOnBright() bool {
	lightness := func(c colorful.Color) float64 {
		l, _, _ := c.Lab()
		return l
	}
	bg := b.Base03
	fg := b.Base0
	bgl := lightness(bg.Color())
	fgl := lightness(fg.Color())
	if fgl == bgl {
		log.Fatalf("bg (%v) and fg (%v) are not supposed to have equal lightness: %v %v", bg, fg, bgl, fgl)
	}
	return fgl > bgl
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
