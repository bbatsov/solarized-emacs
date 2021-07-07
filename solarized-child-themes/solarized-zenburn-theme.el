;;; solarized-zenburn-theme.el --- Solarized Theme  -*- lexical-binding: t -*-

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
;; The dark variant of the solarized theme with a zenburn palette.
;;
;;; Code:

(require 'solarized)

(deftheme solarized-zenburn
  "The dark variant of the Solarized colour theme with gruvbox color palette")

(defvar solarized-zenburn-color-palette-alist
  '(;; zenburn palette
    (base03      . "#3F3F3F")
    (base02      . "#4F4F4F")
    (base01      . "#878777")
    (base00      . "#6F6F6F")
    (base0       . "#DCDCCC")
    (base1       . "#FFFFEF")
    (base2       . "#fffff6")
    (base3       . "#FFFFFD")
    (yellow      . "#F0DFAF")
    (orange      . "#DFAF8F")
    (red         . "#CC9393")
    (magenta     . "#DC8CC3")
    (violet      . "#bbb0cb")
    (blue        . "#8CD0D3")
    (cyan        . "#93E0E3")
    (green       . "#7F9F7F")
    (yellow-1bg  . "#55524c")
    (yellow-1fg  . "#f2e6c3")
    (yellow-2bg  . "#777160")
    (yellow-2fg  . "#ece2c7")
    (yellow-d    . "#b6a576")
    (yellow-l    . "#fff7c4")
    (orange-1bg  . "#534c48")
    (orange-1fg  . "#e7c4ac")
    (orange-2bg  . "#726054")
    (orange-2fg  . "#e4c7b4")
    (orange-d    . "#ac7b5a")
    (orange-l    . "#ffcaa5")
    (red-1bg     . "#504948")
    (red-1fg     . "#dab0af")
    (red-2bg     . "#6b5656")
    (red-2fg     . "#d9b8b6")
    (red-d       . "#9f5c5c")
    (red-l       . "#f4a9a6")
    (magenta-1bg . "#52484f")
    (magenta-1fg . "#e5acd1")
    (magenta-2bg . "#705467")
    (magenta-2fg . "#e2b5d2")
    (magenta-d   . "#aa5790")
    (magenta-l   . "#ffa6e0")
    (violet-1bg  . "#4e4c50")
    (violet-1fg  . "#ccc4d7")
    (violet-2bg  . "#64606a")
    (violet-2fg  . "#cec8d6")
    (violet-d    . "#85749c")
    (violet-l    . "#d5c3ec")
    (blue-1bg    . "#495051")
    (blue-1fg    . "#addbdd")
    (blue-2bg    . "#556c6c")
    (blue-2fg    . "#b7dada")
    (blue-d      . "#57a2a4")
    (blue-l      . "#a9f4f5")
    (cyan-1bg    . "#4a5253")
    (cyan-1fg    . "#b3e7e8")
    (cyan-2bg    . "#577172")
    (cyan-2fg    . "#bbe3e3")
    (cyan-d      . "#5dacaf")
    (cyan-l      . "#b0ffff")
    (green-1bg   . "#464a46")
    (green-1fg   . "#a2b8a1")
    (green-2bg   . "#4f5a4e")
    (green-2fg   . "#acbeab")
    (green-d     . "#488249")
    (green-l     . "#95d291")
    ;; palette end
    )
  "The solarized color palette alist.")

(solarized-with-color-variables 'dark 'solarized-zenburn
  solarized-zenburn-color-palette-alist)

(provide-theme 'solarized-zenburn)

(provide 'solarized-zenburn-theme)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; solarized-zenburn-theme.el ends here
