# colorlab

This utility generates color palette lists for use in themes.

`color.el` can be a bit cumbersome and it *might* not have every color feature
we want so for the sake of easy experimentation it's written in Go because I
found a more fully featured color package to work with.

When we know more about which color functionality is actually required we might
port missing features and translate color palette generation into elisp as well.

This utility is currently not documented or very user friendly since it's an
internal tool under development.

The utility requires at least go 1.13

`go run main.go` will rewrite all palettes in `../solarized-palettes.el` when run.


See https://godoc.org/github.com/bbatsov/solarized-emacs/colorlab
