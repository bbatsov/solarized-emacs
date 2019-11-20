package sol

import "github.com/bbatsov/solarized-emacs/colorlab/pkg/clab"

type Solarized struct {
	Base
	Accents
}

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

func (s Solarized) NamedColors() clab.NamedColors {
	return clab.Merge(s.Base.NamedColors(), s.Accents.NamedColors())
}
