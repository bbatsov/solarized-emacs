;;; solarized-dark-high-contrast-theme.el --- Solarized Light Theme for Emacs  -*- lexical-binding: t -*-

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

(deftheme solarized-dark-high-contrast
  "The dark variant of the Solarized colour theme with slightly higher text contrast")

(defvar solarized-dark-high-contrast-palette-alist
  '(;; solarized-dark-high-contrast palette
    (base03      . "#002732")
    (base02      . "#01323d")
    (base01      . "#62787f")
    (base00      . "#60767e")
    (base0       . "#8d9fa1")
    (base1       . "#9eacac")
    (base2       . "#faf3e0")
    (base3       . "#ffffee")
    (yellow      . "#c49619")
    (orange      . "#db5823")
    (red         . "#ec423a")
    (magenta     . "#e2468f")
    (violet      . "#7a7ed2")
    (blue        . "#3c98e0")
    (cyan        . "#3cafa5")
    (green       . "#93a61a")
    (yellow-1bg  . "#273430")
    (yellow-1fg  . "#bb9a4c")
    (yellow-2bg  . "#484224")
    (yellow-2fg  . "#bba165")
    (yellow-d    . "#936d00")
    (yellow-l    . "#f3be51")
    (orange-1bg  . "#2d2d2f")
    (orange-1fg  . "#d0744d")
    (orange-2bg  . "#522f22")
    (orange-2fg  . "#cd8464")
    (orange-d    . "#a72e01")
    (orange-l    . "#ff8148")
    (red-1bg     . "#2f2c31")
    (red-1fg     . "#dc6a5a")
    (red-2bg     . "#582b29")
    (red-2fg     . "#d77e6f")
    (red-d       . "#ae1212")
    (red-l       . "#ff7254")
    (magenta-1bg . "#292d3c")
    (magenta-1fg . "#d16c96")
    (magenta-2bg . "#512c46")
    (magenta-2fg . "#cd809f")
    (magenta-d   . "#a81761")
    (magenta-l   . "#ff75ab")
    (violet-1bg  . "#103145")
    (violet-1fg  . "#8589c4")
    (violet-2bg  . "#213a5e")
    (violet-2fg  . "#9095c4")
    (violet-d    . "#3548a2")
    (violet-l    . "#9b94f2")
    (blue-1bg    . "#023447")
    (blue-1fg    . "#649bce")
    (blue-2bg    . "#004363")
    (blue-2fg    . "#78a3cb")
    (blue-d      . "#0069b0")
    (blue-l      . "#7ebaff")
    (cyan-1bg    . "#03373f")
    (cyan-1fg    . "#60aca4")
    (cyan-2bg    . "#004b4f")
    (cyan-2fg    . "#73b0aa")
    (cyan-d      . "#008981")
    (cyan-l      . "#77dece")
    (green-1bg   . "#1e3531")
    (green-1fg   . "#97a54d")
    (green-2bg   . "#354725")
    (green-2fg   . "#9eaa66")
    (green-d     . "#687f00")
    (green-l     . "#c3d255")
    ;; palette end
    )
  "The solarized color palette alist.")

(solarized-with-color-variables 'dark 'solarized-dark-high-contrast
  solarized-dark-high-contrast-palette-alist)

(provide-theme 'solarized-dark-high-contrast)


(provide 'solarized-dark-high-contrast-theme)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; solarized-dark-high-contrast-theme.el ends here
