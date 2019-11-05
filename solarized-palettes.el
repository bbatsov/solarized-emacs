;;; solarized-palettes.el

;; see solarized.el for general package header information

;;; Commentary:

;; This file only contains data which is lists of colors of some sort, there is
;; also code that generates palettes in solarized.el. That code does not belong
;; here.

;;; Code:

(defvar solarized-color-palette-alist
  '(;; solarized palette
    (base03    . "#002b36")
    (base02    . "#073642")
    (base01    . "#586e75")
    (base00    . "#657b83")
    (base0     . "#839496")
    (base1     . "#93a1a1")
    (base2     . "#eee8d5")
    (base3     . "#fdf6e3")
    (yellow    . "#b58900")
    (orange    . "#cb4b16")
    (red       . "#dc322f")
    (magenta   . "#d33682")
    (violet    . "#6c71c4")
    (blue      . "#268bd2")
    (cyan      . "#2aa198")
    (green     . "#859900")
    (yellow-d  . "#7B6000")
    (yellow-l  . "#DEB542")
    (orange-d  . "#8B2C02")
    (orange-l  . "#F2804F")
    (red-d     . "#990A1B")
    (red-l     . "#FF6E64")
    (magenta-d . "#93115C")
    (magenta-l . "#F771AC")
    (violet-d  . "#3F4D91")
    (violet-l  . "#9EA0E5")
    (blue-d    . "#00629D")
    (blue-l    . "#69B7F0")
    (cyan-d    . "#00736F")
    (cyan-l    . "#69CABF")
    (green-d   . "#546E00")
    (green-l   . "#B4C342")
    ;; palette end
    )
  "The solarized color palette alist.")

;;; gruvbox themes

(defvar solarized-gruvbox-colors
  '(;; gruvbox colors
    (dark0_hard    . "#1d2021")
    (dark0         . "#282828")
    (dark0_soft    . "#32302f")
    (dark1         . "#3c3836")
    (dark2         . "#504945")
    (dark3         . "#665c54")
    (dark4         . "#7c6f64")
    (gray          . "#928374")
    (light0_hard   . "#f9f5d7")
    (light0        . "#fbf1c7")
    (light0_soft   . "#f2e5bc")
    (light1        . "#ebdbb2")
    (light2        . "#d5c4a1")
    (light3        . "#bdae93")
    (light4        . "#a89984")
    (red           . "#cc241d")
    (green         . "#98971a")
    (yellow        . "#d79921")
    (blue          . "#458588")
    (purple        . "#b16286")
    (aqua          . "#689d6a")
    (orange        . "#d65d0e")
    (bright_red    . "#fb4933")
    (bright_green  . "#b8bb26")
    (bright_yellow . "#fabd2f")
    (bright_blue   . "#83a598")
    (bright_purple . "#d3869b")
    (bright_aqua   . "#8ec07c")
    (bright_orange . "#fe8019")
    (dark_red      . "#9d0006")
    (dark_green    . "#79740e")
    (dark_yellow   . "#b57614")
    (dark_blue     . "#076678")
    (dark_purple   . "#8f3f71")
    (dark_aqua     . "#427b58")
    (dark_orange   . "#af3a03")
    ;; colors end
    )
  "The gruvbox color palette.")

(defvar solarized-gruvbox-dark-color-palette-alist
  '(;; gruvbox-dark palette
    (base00  . (alist-get 'dark4 solarized-gruvbox-colors))
    (base01  . (alist-get 'dark3 solarized-gruvbox-colors))
    (base02  . (alist-get 'dark0_soft solarized-gruvbox-colors))
    (base03  . (alist-get 'dark0 solarized-gruvbox-colors))
    (base0   . (alist-get 'light4 solarized-gruvbox-colors))
    (base1   . (alist-get 'light3 solarized-gruvbox-colors))
    (base2   . (alist-get 'light4 solarized-gruvbox-colors))
    (base3   . (alist-get 'light0 solarized-gruvbox-colors))
    (yellow  . (alist-get 'yellow solarized-gruvbox-colors))
    (orange  . (alist-get 'orange solarized-gruvbox-colors))
    (red     . (alist-get 'bright_red solarized-gruvbox-colors))
    (magenta . (alist-get 'bright_purple solarized-gruvbox-colors))
    (violet  . (alist-get 'purple solarized-gruvbox-colors))
    (blue    . (alist-get 'bright_blue solarized-gruvbox-colors))
    (cyan    . (alist-get 'aqua solarized-gruvbox-colors))
    (green   . (alist-get 'green solarized-gruvbox-colors))

    (yellow-d  . (solarized-color-blend (alist-get 'dark_yellow    solarized-gruvbox-colors) (alist-get 'dark0  solarized-gruvbox-colors) 0.80 2))
    (yellow-l  . (solarized-color-blend (alist-get 'bright_yellow  solarized-gruvbox-colors) (alist-get 'light0 solarized-gruvbox-colors) 0.80 2))
    (orange-d  . (solarized-color-blend (alist-get 'dark_orange    solarized-gruvbox-colors) (alist-get 'dark0  solarized-gruvbox-colors) 0.80 2))
    (orange-l  . (solarized-color-blend (alist-get 'bright_orange  solarized-gruvbox-colors) (alist-get 'light0 solarized-gruvbox-colors) 0.80 2))
    (red-d     . (solarized-color-blend (alist-get 'dark_red       solarized-gruvbox-colors) (alist-get 'dark0  solarized-gruvbox-colors) 0.80 2))
    (red-l     . (solarized-color-blend (alist-get 'bright_red     solarized-gruvbox-colors) (alist-get 'light0 solarized-gruvbox-colors) 0.80 2))
    (magenta-d . (solarized-color-blend (alist-get 'dark_purple    solarized-gruvbox-colors) (alist-get 'dark0  solarized-gruvbox-colors) 0.80 2))
    (magenta-l . (solarized-color-blend (alist-get 'bright_purple  solarized-gruvbox-colors) (alist-get 'light0 solarized-gruvbox-colors) 0.80 2))
    (violet-d  . (solarized-color-blend (alist-get 'dark_purple    solarized-gruvbox-colors) (alist-get 'dark0  solarized-gruvbox-colors) 0.80 2))
    (violet-l  . (solarized-color-blend (alist-get 'bright_purple  solarized-gruvbox-colors) (alist-get 'light0 solarized-gruvbox-colors) 0.80 2))
    (blue-d    . (solarized-color-blend (alist-get 'dark_blue      solarized-gruvbox-colors) (alist-get 'dark0  solarized-gruvbox-colors) 0.80 2))
    (blue-l    . (solarized-color-blend (alist-get 'bright_blue    solarized-gruvbox-colors) (alist-get 'light0 solarized-gruvbox-colors) 0.80 2))
    (cyan-d    . (solarized-color-blend (alist-get 'dark_aqua      solarized-gruvbox-colors) (alist-get 'dark0  solarized-gruvbox-colors) 0.80 2))
    (cyan-l    . (solarized-color-blend (alist-get 'bright_aqua    solarized-gruvbox-colors) (alist-get 'light0 solarized-gruvbox-colors) 0.80 2))
    (green-d   . (solarized-color-blend (alist-get 'dark_green     solarized-gruvbox-colors) (alist-get 'dark0  solarized-gruvbox-colors) 0.80 2))
    (green-l   . (solarized-color-blend (alist-get 'bright_green   solarized-gruvbox-colors) (alist-get 'light0 solarized-gruvbox-colors) 0.80 2))
    ;; palette end
    )
  "The solarized gruvbox dark palette color alist.")

(defvar solarized-gruvbox-light-color-palette-alist
  '(;; gruvbox-light palette
    (base00  . (alist-get 'dark4 solarized-gruvbox-colors))
    (base01  . (alist-get 'dark3 solarized-gruvbox-colors))
    (base02  . (alist-get 'dark0_soft solarized-gruvbox-colors))
    (base03  . (alist-get 'dark0 solarized-gruvbox-colors))
    (base0   . (alist-get 'dark1 solarized-gruvbox-colors))
    (base1   . (alist-get 'light4 solarized-gruvbox-colors))
    (base2   . (alist-get 'light1 solarized-gruvbox-colors))
    (base3   . (alist-get 'light0 solarized-gruvbox-colors))
    (yellow  . (alist-get 'dark_yellow solarized-gruvbox-colors))
    (orange  . (alist-get 'dark_orange solarized-gruvbox-colors))
    (red     . (alist-get 'dark_red solarized-gruvbox-colors))
    (magenta . (alist-get 'bright_purple solarized-gruvbox-colors))
    (violet  . (alist-get 'dark_purple solarized-gruvbox-colors))
    (blue    . (alist-get 'blue solarized-gruvbox-colors))
    (cyan    . (alist-get 'aqua solarized-gruvbox-colors))
    (green   . (alist-get 'green solarized-gruvbox-colors))

    (yellow-d  . (solarized-color-blend (alist-get 'dark_yellow    solarized-gruvbox-colors) (alist-get 'dark0  solarized-gruvbox-colors) 0.80 2))
    (yellow-l  . (solarized-color-blend (alist-get 'bright_yellow  solarized-gruvbox-colors) (alist-get 'light0 solarized-gruvbox-colors) 0.80 2))
    (orange-d  . (solarized-color-blend (alist-get 'dark_orange    solarized-gruvbox-colors) (alist-get 'dark0  solarized-gruvbox-colors) 0.80 2))
    (orange-l  . (solarized-color-blend (alist-get 'bright_orange  solarized-gruvbox-colors) (alist-get 'light0 solarized-gruvbox-colors) 0.80 2))
    (red-d     . (solarized-color-blend (alist-get 'dark_red       solarized-gruvbox-colors) (alist-get 'dark0  solarized-gruvbox-colors) 0.80 2))
    (red-l     . (solarized-color-blend (alist-get 'bright_red     solarized-gruvbox-colors) (alist-get 'light0 solarized-gruvbox-colors) 0.80 2))
    (magenta-d . (solarized-color-blend (alist-get 'dark_purple    solarized-gruvbox-colors) (alist-get 'dark0  solarized-gruvbox-colors) 0.80 2))
    (magenta-l . (solarized-color-blend (alist-get 'bright_purple  solarized-gruvbox-colors) (alist-get 'light0 solarized-gruvbox-colors) 0.80 2))
    (violet-d  . (solarized-color-blend (alist-get 'dark_purple    solarized-gruvbox-colors) (alist-get 'dark0  solarized-gruvbox-colors) 0.80 2))
    (violet-l  . (solarized-color-blend (alist-get 'bright_purple  solarized-gruvbox-colors) (alist-get 'light0 solarized-gruvbox-colors) 0.80 2))
    (blue-d    . (solarized-color-blend (alist-get 'dark_blue      solarized-gruvbox-colors) (alist-get 'dark0  solarized-gruvbox-colors) 0.80 2))
    (blue-l    . (solarized-color-blend (alist-get 'bright_blue    solarized-gruvbox-colors) (alist-get 'light0 solarized-gruvbox-colors) 0.80 2))
    (cyan-d    . (solarized-color-blend (alist-get 'dark_aqua      solarized-gruvbox-colors) (alist-get 'dark0  solarized-gruvbox-colors) 0.80 2))
    (cyan-l    . (solarized-color-blend (alist-get 'bright_aqua    solarized-gruvbox-colors) (alist-get 'light0 solarized-gruvbox-colors) 0.80 2))
    (green-d   . (solarized-color-blend (alist-get 'dark_green     solarized-gruvbox-colors) (alist-get 'dark0  solarized-gruvbox-colors) 0.80 2))
    (green-l   . (solarized-color-blend (alist-get 'bright_green   solarized-gruvbox-colors) (alist-get 'light0 solarized-gruvbox-colors) 0.80 2))
    ;; palette end
    )
  "The solarized gruvbox light color palette alist.")

(provide 'solarized-palettes)
;; End:

;;; solarized-palettes.el ends here
