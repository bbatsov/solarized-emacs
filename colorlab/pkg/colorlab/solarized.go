package colorlab

import (
	"github.com/lucasb-eyer/go-colorful"
)

type Solarized struct {
	Base
	Accents
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
