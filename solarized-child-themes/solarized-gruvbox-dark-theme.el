;;; solarized-gruvbox-dark-theme.el --- Solarized Theme  -*- lexical-binding: t -*-

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
;; The dark variant of the solarized theme.
;;
;;; Code:

(require 'solarized)

(deftheme solarized-gruvbox-dark
  "The dark variant of the Solarized colour theme with gruvbox color palette")

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
    (base03      . "#282828")
    (base02      . "#32302f")
    (base01      . "#7c6f64")
    (base00      . "#282828")
    (base0       . "#a89984")
    (base1       . "#bdae93")
    (base2       . "#a89984")
    (base3       . "#fbf1c7")
    (yellow      . "#d79921")
    (orange      . "#d65d0e")
    (red         . "#fb4933")
    (magenta     . "#d3869b")
    (violet      . "#b16286")
    (blue        . "#458588")
    (cyan        . "#689d6a")
    (green       . "#98971a")
    (yellow-1bg  . "#3f3528")
    (yellow-1fg  . "#e2b055")
    (yellow-2bg  . "#614923")
    (yellow-2fg  . "#dfb566")
    (yellow-d    . "#a76e00")
    (yellow-l    . "#f3ac41")
    (orange-1bg  . "#402e25")
    (orange-1fg  . "#e48847")
    (orange-2bg  . "#62351c")
    (orange-2fg  . "#e2965a")
    (orange-d    . "#a53600")
    (orange-l    . "#f37535")
    (red-1bg     . "#452e28")
    (red-1fg     . "#ff815b")
    (red-2bg     . "#6f3125")
    (red-2fg     . "#f89169")
    (red-d       . "#b21b0a")
    (red-l       . "#ff6540")
    (magenta-1bg . "#3d3335")
    (magenta-1fg . "#dda3a6")
    (magenta-2bg . "#5e434a")
    (magenta-2fg . "#dbaba4")
    (magenta-d   . "#9f4d64")
    (magenta-l   . "#e78c9e")
    (violet-1bg  . "#392f33")
    (violet-1fg  . "#c68a97")
    (violet-2bg  . "#523642")
    (violet-2fg  . "#c89798")
    (violet-d    . "#8b2a58")
    (violet-l    . "#d36b91")
    (blue-1bg    . "#2c3333")
    (blue-1fg    . "#7da298")
    (blue-2bg    . "#2d4243")
    (blue-2fg    . "#90aa99")
    (blue-d      . "#14676b")
    (blue-l      . "#63a6a5")
    (cyan-1bg    . "#2f362f")
    (cyan-1fg    . "#91b382")
    (cyan-2bg    . "#384a38")
    (cyan-2fg    . "#9fb888")
    (cyan-d      . "#2e7d33")
    (cyan-l      . "#75bf6b")
    (green-1bg   . "#363527")
    (green-1fg   . "#b4ae51")
    (green-2bg   . "#4a4821")
    (green-2fg   . "#bab462")
    (green-d     . "#747400")
    (green-l     . "#b9b340")
    ;; palette end
    )
  "The solarized gruvbox dark palette color alist.")

(solarized-with-color-variables 'dark 'solarized-gruvbox-dark
  solarized-gruvbox-dark-color-palette-alist)

(provide-theme 'solarized-gruvbox-dark)

(provide 'solarized-gruvbox-dark-theme)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; solarized-gruvbox-dark-theme.el ends here
