;;; solarized-kanagawa-lotus-theme.el --- Solarized Theme  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Thomas Frössman

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
;; The Solarized theme with the Kanagawa lotus palette.
;;
;; The palette is Kanagawa by Tommaso Laurenzi (MIT License),
;; https://github.com/rebelot/kanagawa.nvim.
;;
;; The palette is stored as the theme draws it, background first, so
;; the theme is created as a dark variant, as solarized-selenized-light is.
;;
;;; Code:

(require 'solarized)
(eval-when-compile
  (require 'solarized-palettes))

(deftheme solarized-kanagawa-lotus
  "The Solarized theme with the Kanagawa lotus palette.")

(solarized-with-color-variables 'dark 'solarized-kanagawa-lotus
  solarized-kanagawa-lotus-color-palette-alist)

(provide-theme 'solarized-kanagawa-lotus)

(provide 'solarized-kanagawa-lotus-theme)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
