# Prelude

Solarized for Emacs is an Emacs port of the [Solarized theme for vim](http://ethanschoonover.com/solarized),
developed by Ethan Schoonover.

Solarized for Emacs is tested only under Emacs 24, but should be
working under Emacs 23 as well. The theme is implemented in terms of
customizations and `deftheme` and does not require the
`color-theme-package`.

# Installation

## Emacs Prelude

Solarized for Emacs is already bundled into
[Emacs Prelude](https://github.com/bbatsov/prelude). If you're a
Prelude user, just do a `M-x load-theme` and pick either
_solarized-dark_ or _solarized-light_.

## Stand-alone installation

1. Download or clone this repository somewhere, for example at `~/.emacs.d/themes/solarized`
2. Add this your `.emacs.d`: `(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized")`
3. Load the theme with the interactive function `load-theme`: `(load-theme 'solarized-dark t)`

## MELPA & Marmalade

Solarized for Emacs is available for installation via the
[MELPA](http://melpa.milkbox.net) and
[Marmalade](http://marmalade-repo.org/) `package.el`
repositories. Assuming you've set one of the them up (I recommend
MELPA) you can install solarized like this:

`M-x package-install solarized-theme`

Afterwards - business as usual, just load one of the theme variants
with `M-x load-theme`. 

# Bugs & Improvements

Please, report any problems that you find on the projects integrated
issue tracker. If you've added some improvements and you want them
included upstream don't hesitate to send me a patch or even better - a
GitHub pull request.

# Contributors

- [Thomas Frössman](http://t.jossystem.se)

(Add yourself to the list)
