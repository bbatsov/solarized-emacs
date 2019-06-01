[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](http://melpa.org/packages/solarized-theme-badge.svg)](http://melpa.org/#/solarized-theme)
[![MELPA Stable](http://stable.melpa.org/packages/solarized-theme-badge.svg)](http://stable.melpa.org/#/solarized-theme)
[![Join the chat at https://gitter.im/bbatsov/solarized-emacs](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/bbatsov/solarized-emacs?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

# Solarized for Emacs

Solarized for Emacs is an Emacs port of the [Solarized theme for vim](http://ethanschoonover.com/solarized),
developed by Ethan Schoonover.

You can find several screenshots of Solarized for Emacs [here](https://emacsthemes.com/themes/solarized-themes.html).

Solarized for Emacs is tested only under Emacs 24, but should be
working under Emacs 23 as well. The theme is implemented in terms of
customizations and `deftheme` and does not require the
`color-theme-package`.

## Installation

Solarized for Emacs is available for installation via the
[MELPA](http://melpa.org)  `package.el`
repository. Assuming you've set it up you can install Solarized like this:

`M-x package-install solarized-theme`

This package will install two variants of the theme - `solarized-light-theme`
and `solarized-dark-theme`. You can load one of the theme variants with `M-x
load-theme`.

To load it automatically on Emacs startup add this to your init file:

```el
(load-theme 'solarized-light t)
```

or

```el
(load-theme 'solarized-dark t)
```

(If you want to install manually that procedure is briefly documented in the
FAQ at the end of this document.)

## Customisations

### Theme specific settings

If you don't like low-contrast modeline or fringe, you can `customize` them
either by doing `M-x customize-group solarized` or setting the values using
elisp code:

```el
;; make the fringe stand out from the background
(setq solarized-distinct-fringe-background t)

;; Don't change the font for some headings and titles
(setq solarized-use-variable-pitch nil)

;; make the modeline high contrast
(setq solarized-high-contrast-mode-line t)

;; Use less bolding
(setq solarized-use-less-bold t)

;; Use more italics
(setq solarized-use-more-italic t)

;; Use less colors for indicators such as git:gutter, flycheck and similar
(setq solarized-emphasize-indicators nil)

;; Don't change size of org-mode headlines (but keep other size-changes)
(setq solarized-scale-org-headlines nil)

;; Avoid all font-size changes
(setq solarized-height-minus-1 1.0)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)

```

Note that these need to be set **before** `load-theme` is invoked for Solarized.

### Underline position setting for X

If you are using Emacs under X you might like the following setting which puts
the underline below the
[font bottomline instead of the baseline](https://publib.boulder.ibm.com/infocenter/pseries/v5r3/topic/com.ibm.aix.graPHIGS/doc/phigstrf/figures/afma5rbd.jpg).

Imho it enhances the general readability and also it fits well with the default
`solarized-high-contrast-mode-line` setting which uses an slightly emphazised
underline for the modeline to create one horizontal window border in the same
manner as the vertical border.

```el
(setq x-underline-at-descent-line t)
```

## Bugs & Improvements

Please, report any problems that you find on the projects integrated
issue tracker. If you've added some improvements and you want them
included upstream don't hesitate to send me a patch or even better - a
GitHub pull request.

## FAQ

### Stand-alone manual installation

Save the following files in a folder that's on your Emacs' `load-path`:

* [dash.el](https://raw.githubusercontent.com/magnars/dash.el/master/dash.el) - [dash](https://github.com/magnars/dash.el), a modern list library for Emacs
* [solarized.el](https://raw.githubusercontent.com/bbatsov/solarized-emacs/master/solarized.el) - the solarized theme

Save the following files into `~/.emacs.d/themes`:

* [solarized-light-theme.el](https://raw.githubusercontent.com/bbatsov/solarized-emacs/master/solarized-light-theme.el)
* [solarized-dark-theme.el](https://raw.githubusercontent.com/bbatsov/solarized-emacs/master/solarized-dark-theme.el)

Add this your `.emacs.d`:

```el
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
```

Now you can load the theme with the interactive function `load-theme`.

## Contributors

- [Thomas Frössman](http://t.jossystem.se)

(Add yourself to the list)

Cheers,<br\>
[Bozhidar](http://twitter.com/bbatsov)

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
