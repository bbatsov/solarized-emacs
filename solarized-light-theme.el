;;; solarized-light-theme.el --- Solarized Light Theme for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2011-2019 Bozhidar Batsov

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
;; The light variant of the solarized theme.
;;
;;; Code:

(require 'solarized)

(deftheme solarized-light "The light variant of the Solarized colour theme")

(defvar solarized-light-color-palette-alist
  '(;; solarized-light palette
    (base03      . "#002b36")
    (base02      . "#073642")
    (base01      . "#586e75")
    (base00      . "#657b83")
    (base0       . "#839496")
    (base1       . "#93a1a1")
    (base2       . "#eee8d5")
    (base3       . "#fdf6e3")
    (yellow      . "#b58900")
    (orange      . "#cb4b16")
    (red         . "#dc322f")
    (magenta     . "#d33682")
    (violet      . "#6c71c4")
    (blue        . "#268bd2")
    (cyan        . "#2aa198")
    (green       . "#859900")
    (yellow-1bg  . "#f8e8c6")
    (yellow-1fg  . "#876d26")
    (yellow-2bg  . "#f1d49b")
    (yellow-2fg  . "#766634")
    (yellow-d    . "#866300")
    (yellow-l    . "#e1af4b")
    (orange-1bg  . "#fedfc5")
    (orange-1fg  . "#974727")
    (orange-2bg  . "#ffbd99")
    (orange-2fg  . "#854a33")
    (orange-d    . "#992700")
    (orange-l    . "#fb7640")
    (red-1bg     . "#ffdec8")
    (red-1fg     . "#a33c35")
    (red-2bg     . "#ffb9a1")
    (red-2fg     . "#8e433d")
    (red-d       . "#a7020a")
    (red-l       . "#ff6849")
    (magenta-1bg . "#fdded7")
    (magenta-1fg . "#9a3f6c")
    (magenta-2bg . "#fdbac6")
    (magenta-2fg . "#854568")
    (magenta-d   . "#a00559")
    (magenta-l   . "#ff699e")
    (violet-1bg  . "#ebe4e2")
    (violet-1fg  . "#4f5e99")
    (violet-2bg  . "#d1c9e3")
    (violet-2fg  . "#475a8b")
    (violet-d    . "#243e9b")
    (violet-l    . "#8d85e7")
    (blue-1bg    . "#e7e8e4")
    (blue-1fg    . "#1e6fa2")
    (blue-2bg    . "#c3d5e9")
    (blue-2fg    . "#246792")
    (blue-d      . "#0061a8")
    (blue-l      . "#74adf5")
    (cyan-1bg    . "#e4ecda")
    (cyan-1fg    . "#207e7b")
    (cyan-2bg    . "#bedfcf")
    (cyan-2fg    . "#247374")
    (cyan-d      . "#007d76")
    (cyan-l      . "#6ccec0")
    (green-1bg   . "#efeac7")
    (green-1fg   . "#657827")
    (green-2bg   . "#dbdb9c")
    (green-2fg   . "#5b6e35")
    (green-d     . "#5b7300")
    (green-l     . "#b3c34d")
    ;; palette end
    )
  "The solarized color palette alist.")

(solarized-with-color-variables 'light 'solarized-light
  solarized-light-color-palette-alist)

(provide-theme 'solarized-light)

(provide 'solarized-light-theme)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; solarized-light-theme.el ends here
