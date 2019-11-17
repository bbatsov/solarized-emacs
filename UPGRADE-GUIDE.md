# solarized theme upgrade guide

This upgrade guide is meant as a way for people who don't read emacs lisp to
adapt to structural changes with minimal effort.

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

(deftheme my-solarized-dark "The light variant of the Solarized colour theme")
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

;; This files needs to be places iside the custom-theme-load-path list

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

;; This files needs to be places iside the custom-theme-load-path list

(deftheme my-solarized-dark "The dark variant of the Solarized colour theme")
(solarized-with-color-variables
  'dark 'my-solarized-dark solarized-dark-color-palette-alist my-solarized-faces)

(provide-theme 'my-solarized-dark)

```
