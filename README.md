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
[Emacs Prelude](https://github.com/bbatsov/emacs-prelude). If you're a
Prelude user, just do a `M-x load-theme` and pick either
_solarized-dark_ or _solarized-light_.

## Stand-alone installation

Download `solarized-theme.el`, `solarized-dark-theme.el` and
`solarized-light-theme.el`.

Place `solarized-theme.el` is a folder that's on your Emacs' `load-path`.
Afterward place `solarized-dark-theme.el` and
`solarized-light-theme.el` in `~/.emacs.d/themes` (or some other
folder if you prefer so). Add this your
`.emacs.d`:

`(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")`

Now you can load the theme with the interactive function `load-theme`.

## Marmalade

Solarized for Emacs is available for installation via the [Marmalade
repo](http://marmalade-repo.org/):

`M-x package-install solarized-theme`

Afterwards - business as usual, just load one of the theme variants
with `M-x load-theme`. 

# Bugs & Improvements

Please, report any problems that you find on the projects integrated
issue tracker. If you've added some improvements and you want them
included upstream don't hesitate to send me a patch or even better - a
GitHub pull request.

# Contributors

So far - none. You could be the first. :-)
