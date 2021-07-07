;;; solarized-gruvbox-light-theme.el --- Solarized Theme  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Thomas Frössman

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The dark variant of the solarized theme with gruvbox color palette
;;
;;; Code:

(require 'solarized)

(deftheme solarized-gruvbox-light
  "The light variant of the Solarized colour theme with gruvbox color palette")

(defvar solarized-gruvbox-light-color-palette-alist
  '(;; gruvbox-light palette
    (base03      . "#282828")
    (base02      . "#32302f")
    (base01      . "#665c54")
    (base00      . "#7c6f64")
    (base0       . "#3c3836")
    (base1       . "#a89984")
    (base2       . "#ebdbb2")
    (base3       . "#fbf1c7")
    (yellow      . "#b57614")
    (orange      . "#af3a03")
    (red         . "#9d0006")
    (magenta     . "#d3869b")
    (violet      . "#8f3f71")
    (blue        . "#076678")
    (cyan        . "#689d6a")
    (green       . "#98971a")
    (yellow-1bg  . "#f6e1af")
    (yellow-1fg  . "#8c6023")
    (yellow-2bg  . "#efc98b")
    (yellow-2fg  . "#7f5b2d")
    (yellow-d    . "#8a5100")
    (yellow-l    . "#e29a3f")
    (orange-1bg  . "#f7d8ab")
    (orange-1fg  . "#893a18")
    (orange-2bg  . "#f0b382")
    (orange-2fg  . "#7e3e23")
    (orange-d    . "#841900")
    (orange-l    . "#df6835")
    (red-1bg     . "#f5d4aa")
    (red-1fg     . "#7e2115")
    (red-2bg     . "#eaa67f")
    (red-2fg     . "#752d21")
    (red-d       . "#750000")
    (red-l       . "#cf5130")
    (magenta-1bg . "#f9e4c3")
    (magenta-1fg . "#9e6b78")
    (magenta-2bg . "#f8d1c0")
    (magenta-2fg . "#8c646e")
    (magenta-d   . "#9f4d64")
    (magenta-l   . "#f598a7")
    (violet-1bg  . "#eed8bd")
    (violet-1fg  . "#713c5c")
    (violet-2bg  . "#dcb3af")
    (violet-2fg  . "#694058")
    (violet-d    . "#6f104d")
    (violet-l    . "#c2608f")
    (blue-1bg    . "#dedebe")
    (blue-1fg    . "#245561")
    (blue-2bg    . "#b1c2b2")
    (blue-2fg    . "#30535c")
    (blue-d      . "#004858")
    (blue-l      . "#5b919b")
    (cyan-1bg    . "#e7e7bb")
    (cyan-1fg    . "#577a58")
    (cyan-2bg    . "#cbdaab")
    (cyan-2fg    . "#556f55")
    (cyan-d      . "#2e7d33")
    (cyan-l      . "#82cc73")
    (green-1bg   . "#f0e6b1")
    (green-1fg   . "#787628")
    (green-2bg   . "#e0d78f")
    (green-2fg   . "#6f6c32")
    (green-d     . "#747400")
    (green-l     . "#c6c148")
    ;; palette end
    )
  "The solarized gruvbox light color palette alist.")

(solarized-with-color-variables 'light 'solarized-gruvbox-light
  solarized-gruvbox-light-color-palette-alist)

(provide-theme 'solarized-gruvbox-light)

(provide 'solarized-gruvbox-light-theme)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; solarized-gruvbox-light-theme.el ends here
