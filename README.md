[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](http://melpa.org/packages/solarized-theme-badge.svg)](http://melpa.org/#/solarized-theme)
[![MELPA Stable](http://stable.melpa.org/packages/solarized-theme-badge.svg)](http://stable.melpa.org/#/solarized-theme)
[![Join the chat at https://gitter.im/bbatsov/solarized-emacs](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/bbatsov/solarized-emacs?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

# Solarized for Emacs

Solarized for Emacs is an Emacs 24bit theme making use of of the
[Solarized palette](http://ethanschoonover.com/solarized), developed by Ethan Schoonover.

You can find several screenshots of Solarized for Emacs
[here](https://emacsthemes.com/themes/solarized-themes.html).

Solarized for Emacs supports officially Emacs 24+, but should be working under
Emacs 23 as well. The theme is implemented in terms of customisations and
`deftheme` and does not require the `color-theme-package`.

## Installation

Solarized for Emacs is available for installation via the
[MELPA](http://melpa.org) using `package.el`.  Assuming you've set it
up you can install Solarized like this:

`M-x package-install solarized-theme`

This package will install two variants of the theme; `solarized-light-theme`
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

## Alternative Palettes

The intent of this theme will always be that Solarized-dark/light will give you
the best possible experience. Palettes other than Solarized will never have
influence over theming decisions, they are complementary.

You should not expect the complementary themes to be fully accurate or the most
suitable versions of how to apply those palettes into an emacs theme. The
important aspect is that the color usage should be about the same as they are
with the default Solarized palette.

Colors stand out with varying amounts depending on which palette is used. This
theme aims for a very soft look, violet is one of the most pronounced accent
colors in the Solarized palette and that is one reason why it's rarely used by
the theme. Violet in another palette might stand out less and therefore be used
more often if the theme was specifically designed with that palette in mind.

The original Zenburn palette doesn't have an violet it is created by blending
blue and magenta to get it to fit into the theme so it's tilted towards
translating into what this theme has decided.

*TLDR*; While `solarized-gruvbox-light` might be good complement for users of
`solarized-light`, another theme not contained in this package (`gruvbox-light`
or whatever) might be more finely tuned in relation to the Gruvbox palette.

## Customisations

### Theme-specific settings

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

;; Change the size of markdown-mode headlines (off by default)
(setq solarized-scale-markdown-headlines t)

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

## Create theme using color palette

The Solarized Face settings consist of a palette of colors with eight
accents in addition to the darkest and brightest colors. Recent
changes allow you to freely create theme files using different
palettes instead of the Solarized color palette.  It consists of two
steps:

* creating a theme file
* loading a theme

### Creating/Loading Theme Files

Select the darkest and lightest colors and the eight accents to pass
to the function. This creates a theme file in `.emacs.d/themes/`. If
you need to make minor modifications, you can override Face
individually by specifying a free Sexp as the fourth argument.

Once you have a theme file, you can load it with `load-theme`, like
solarized.

```el
;; inspired vim's jellybeans color-theme
(solarized-create-theme-file-with-palette 'light 'solarized-jellybeans-light
  '("#202020" "#ffffff"
    "#ffb964" "#8fbfdc" "#a04040" "#b05080" "#805090" "#fad08a" "#99ad6a" "#8fbfdc"))

(load-theme 'solarized-jellybeans-light t)
```

```el
;; inspired emacs's mesa color-theme
(solarized-create-theme-file-with-palette 'light 'solarized-mesa-light
  '("#000000" "#faf5ee"
    "#3388dd" "#ac3d1a" "#dd2222" "#8b008b" "#00b7f0" "#1388a2" "#104e8b" "#00688b"))

(load-theme 'solarized-mesa-light t)
```

```el
;; inspired emacs's solarized color-theme
(solarized-create-theme-file-with-palette 'light 'solarized-solarized-light
  '("#002b36" "#fdf6e3"
    "#b58900" "#cb4b16" "#dc322f" "#d33682" "#6c71c4" "#268bd2" "#2aa198" "#859900"))

(load-theme 'solarized-solarized-light t)
```

```el
;; wombat color-theme with misc face definition
(solarized-create-theme-file-with-palette 'dark 'solarized-wombat-dark
  '("#2a2a29" "#f6f3e8"
    "#e5c06d" "#ddaa6f" "#ffb4ac" "#e5786d" "#834c98" "#a4b5e6" "#7ec98f" "#8ac6f2")
  '((custom-theme-set-faces
     theme-name
     `(default ((,class (:foreground ,(solarized-color-blend base03 base3 0.15 2) :background ,base03))))
     `(highlight ((,class (:background ,violet))))
     `(font-lock-builtin-face ((,class (:foreground ,magenta))))
     `(font-lock-constant-face ((,class (:foreground ,blue))))
     `(font-lock-comment-face ((,class (:foreground ,base00))))
     `(mode-line
       ((,class (:foreground ,base2 :background ,(solarized-color-blend base03 base3 0.85 2)))))
     `(mode-line-inactive
       ((,class (:foreground ,base00 :background ,(solarized-color-blend base03 "black" 0.85 2)))))
     `(mode-line-buffer-id ((,class (:foreground ,base3 :weight bold))))
     `(minibuffer-prompt ((,class (:foreground ,base1))))
     `(vertical-border ((,class (:foreground ,base03)))))))

(load-theme 'solarized-wombat-dark t)
```

**Note:** If the theme file already exists, `solorized-create-theme-file` does not
regenerate the file. If the file exists, it can be overwritten by
setting the fifth argument to `t`.

## Versioning Policy

- This package uses [semantic versioning](https://semver.org/)

- Master branch is to be considered unstable (which isn't an excuse to
  constantly break behaviour). Use MELPA Stable if you want fewer breaking
  changes.

- Next stable version will be 2.0 because of API changes in the master branch.
  Regular `load-theme` usage is unaffected by changes.

## Bugs & Improvements

Please, report any problems that you find on the projects integrated
issue tracker. If you've added some improvements and you want them
included upstream don't hesitate to send me a patch or even better - a
GitHub pull request.

## License

Copyright © 2011-2021 Bozhidar Batsov, [Thomas Frössman](http://t.jossystem.se), and
[contributors](https://github.com/bbatsov/solarized-emacs/contributors).

Distributed under the GNU General Public License, version 3

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
