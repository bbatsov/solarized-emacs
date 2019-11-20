package colorlab

import "github.com/lucasb-eyer/go-colorful"

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
	return HexColor(c1.BlendLab(c2, t).Clamped().Hex())
}

func NewHexColor(c colorful.Color) HexColor {
	return HexColor(c.Clamped().Hex())
}
