;;; solarized-light-high-contrast-theme.el --- Solarized Light Theme for Emacs  -*- lexical-binding: t -*-

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
;; The light variant of the solarized theme.
;;
;;; Code:

(require 'solarized)

(deftheme solarized-light-high-contrast
  "The light variant of the Solarized colour theme with slightly higher text contrast")

(defvar solarized-light-high-contrast-palette-alist
  '(;; solarized-light-high-contrast palette
    (base03      . "#00212b")
    (base02      . "#002b37")
    (base01      . "#5d737a")
    (base00      . "#596e76")
    (base0       . "#88999b")
    (base1       . "#98a6a6")
    (base2       . "#f4eedb")
    (base3       . "#fffce9")
    (yellow      . "#a67c00")
    (orange      . "#bb3e06")
    (red         . "#cc1f24")
    (magenta     . "#c42475")
    (violet      . "#5e65b6")
    (blue        . "#007ec4")
    (cyan        . "#11948b")
    (green       . "#778c00")
    (yellow-1bg  . "#f7ebcb")
    (yellow-1fg  . "#7a6120")
    (yellow-2bg  . "#ecd29c")
    (yellow-2fg  . "#6b5a2c")
    (yellow-d    . "#785700")
    (yellow-l    . "#d6a549")
    (orange-1bg  . "#fee2c8")
    (orange-1fg  . "#8a3b1c")
    (orange-2bg  . "#fabc97")
    (orange-2fg  . "#793e28")
    (orange-d    . "#891b00")
    (orange-l    . "#ed6e3e")
    (red-1bg     . "#ffe1cb")
    (red-1fg     . "#952f2a")
    (red-2bg     . "#ffb79f")
    (red-2fg     . "#823731")
    (red-d       . "#990001")
    (red-l       . "#ff6243")
    (magenta-1bg . "#fce1da")
    (magenta-1fg . "#8e3160")
    (magenta-2bg . "#f9b9c4")
    (magenta-2fg . "#7a395c")
    (magenta-d   . "#93004d")
    (magenta-l   . "#f46495")
    (violet-1bg  . "#ebe7e5")
    (violet-1fg  . "#44528c")
    (violet-2bg  . "#cdc8e1")
    (violet-2fg  . "#3d4f7e")
    (violet-d    . "#11328f")
    (violet-l    . "#837bdf")
    (blue-1bg    . "#e6ebe7")
    (blue-1fg    . "#0a6395")
    (blue-2bg    . "#bfd2e6")
    (blue-2fg    . "#185b85")
    (blue-d      . "#005797")
    (blue-l      . "#6fa5e7")
    (cyan-1bg    . "#e4efdd")
    (cyan-1fg    . "#10716f")
    (cyan-2bg    . "#b9ddcd")
    (cyan-2fg    . "#196767")
    (cyan-d      . "#006d68")
    (cyan-l      . "#66c1b3")
    (green-1bg   . "#eeedcb")
    (green-1fg   . "#596c21")
    (green-2bg   . "#d5d99d")
    (green-2fg   . "#51622d")
    (green-d     . "#4f6600")
    (green-l     . "#a8b84b")
    ;; palette end
    )
  "The solarized color palette alist.")

(solarized-with-color-variables 'light 'solarized-light-high-contrast
  solarized-light-high-contrast-palette-alist)

(provide-theme 'solarized-light-high-contrast)

(provide 'solarized-light-high-contrast-theme)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; solarized-light-high-contrast-theme.el ends here
