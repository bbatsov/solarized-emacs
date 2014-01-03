# Solarized for Emacs

Solarized for Emacs is an Emacs port of the [Solarized theme for vim](http://ethanschoonover.com/solarized),
developed by Ethan Schoonover.

Solarized for Emacs is tested only under Emacs 24, but should be
working under Emacs 23 as well. The theme is implemented in terms of
customizations and `deftheme` and does not require the
`color-theme-package`.

# Installation

## Stand-alone installation

Download `solarized.el`, `solarized-dark-theme.el` and
`solarized-light-theme.el`.

Place `solarized.el` in a folder that's on your Emacs' `load-path`.
Afterward place `solarized-dark-theme.el` and
`solarized-light-theme.el` in `~/.emacs.d/themes` (or some other
folder if you prefer so). Add this your
`.emacs.d`:

```lisp
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
```

Now you can load the theme with the interactive function `load-theme`.

## MELPA & Marmalade

Solarized for Emacs is available for installation via the
[MELPA](http://melpa.milkbox.net) and
[Marmalade](http://marmalade-repo.org/) `package.el`
repositories. Assuming you've set one of the them up (I recommend
MELPA) you can install solarized like this:

`M-x package-install solarized-theme`

Afterwards - business as usual, just load one of the theme variants
with `M-x load-theme`.

# Customisations

If you don't like low-contrast modeline or fringe, you can `customize` them either by doing `M-x customize-group solarized` or setting the values using elisp code:
```lisp
;; make the fringe stand out from the background
(setq solarized-distinct-fringe-background t)

;; make the modeline high contrast
(setq solarized-high-contrast-mode-line t)
```

Note that these need to be set **before** `load-theme` is invoked for Solarized.

# Bugs & Improvements

Please, report any problems that you find on the projects integrated
issue tracker. If you've added some improvements and you want them
included upstream don't hesitate to send me a patch or even better - a
GitHub pull request.

# Contributors

- [Thomas Frössman](http://t.jossystem.se)

(Add yourself to the list)
