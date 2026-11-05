# solarized theme upgrade guide

This upgrade guide is meant as a way for people who don't read emacs lisp to
adapt to structural changes with minimal effort.

## 2026-11-05 - light palettes are declared as light

The light palettes are now loaded with the `'light` variant instead of `'dark`,
like `solarized-light`.

If you have a child theme built on one of these palettes, change the first
argument of `solarized-with-color-variables` from `'dark` to `'light`:

- `solarized-selenized-light-color-palette-alist`
- `solarized-selenized-white-color-palette-alist`
- `solarized-everforest-light-color-palette-alist`
- `solarized-flexoki-light-color-palette-alist`
- `solarized-kanagawa-lotus-color-palette-alist`
- `solarized-dayfox-color-palette-alist`
- `solarized-dawnfox-color-palette-alist`
- `solarized-carbon-light-color-palette-alist`
- `solarized-apple-light-color-palette-alist`

If you don't, the background and foreground come out swapped (a dark
background with light text, or washed-out colors).

### before

**themes/my-solarized-selenized-light-theme.el**
```el
(deftheme my-solarized-selenized-light "The light variant of the Solarized colour theme")
(solarized-with-color-variables
  'dark 'my-solarized-selenized-light solarized-selenized-light-color-palette-alist my-solarized-faces)
```

### after

**themes/my-solarized-selenized-light-theme.el**
```el
(deftheme my-solarized-selenized-light "The light variant of the Solarized colour theme")
(solarized-with-color-variables
  'light 'my-solarized-selenized-light solarized-selenized-light-color-palette-alist my-solarized-faces)
```

If you made your own light palette by copying one of the palettes above, swap
its bases the same way (`base03`↔`base3`, `base02`↔`base2`, `base01`↔`base1`,
`base00`↔`base0`) and load it with `'light`.

## 2019-11-17 - child theme usage

This is an example of how to adapt derived child themes to the changes:

### before

**my-solarized.el**

```el
(require 'solarized)

(defun my-solarized-theme ()
  "My personal solarized theme customization."

  (custom-theme-set-faces
   theme-name
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,base02)))))

  (custom-theme-set-variables
   theme-name
   `(org-todo-keyword-faces
     (quote (("TODO" :foreground ,cyan :weight bold :slant italic :underline nil))))))

(provide 'my-solarized)
```

**themes/my-solarized-light-theme.el**
```el
(require 'solarized)
(require 'my-solarized)

(deftheme my-solarized-light "The light variant of the Solarized colour theme")
(create-solarized-theme 'light 'my-solarized-light 'my-solarized-theme)

(provide-theme 'my-solarized-light)
```

**themes/my-solarized-dark-theme.el**
```el
(require 'solarized)
(require 'my-solarized)

(deftheme my-solarized-dark "The dark variant of the Solarized colour theme")
(create-solarized-theme 'dark 'my-solarized-dark 'my-solarized-theme)

(provide-theme 'my-solarized-dark)
```


### after

**my-solarized.el**
```el
(require 'solarized)

(setq my-solarized-faces
      '("My personal solarized theme customization."
        (custom-theme-set-faces
         theme-name
         `(rainbow-delimiters-depth-1-face ((,class (:foreground ,base02)))))
        (custom-theme-set-variables
         theme-name
         `(org-todo-keyword-faces
           (quote (("TODO" :foreground ,cyan :weight bold :slant italic :underline nil)))))))

(provide 'my-solarized)

```


**themes/my-solarized-light-theme.el**

```el
(require 'solarized)
(require 'my-solarized)
(eval-when-compile
  (require 'solarized-palettes))

;; This file needs to be placed inside the custom-theme-load-path list

(deftheme my-solarized-light "The light variant of the Solarized colour theme")
(solarized-with-color-variables
  'light 'my-solarized-light solarized-light-color-palette-alist my-solarized-faces)

(provide-theme 'my-solarized-light)

```

**my-solarized-dark-theme.el**

```el
(require 'solarized)
(require 'my-solarized)
(eval-when-compile
  (require 'solarized-palettes))

;; This file needs to be placed inside the custom-theme-load-path list

(deftheme my-solarized-dark "The dark variant of the Solarized colour theme")
(solarized-with-color-variables
  'dark 'my-solarized-dark solarized-dark-color-palette-alist my-solarized-faces)

(provide-theme 'my-solarized-dark)

```
