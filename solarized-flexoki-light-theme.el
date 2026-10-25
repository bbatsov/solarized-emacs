;;; solarized-flexoki-light-theme.el --- Solarized Theme  -*- lexical-binding: t -*-

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
;; The Solarized theme with the Flexoki light palette.
;;
;; The palette is Flexoki by Steph Ango (MIT License),
;; https://stephango.com/flexoki.
;;
;; The palette is stored as the theme draws it, background first, so
;; the theme is created as a dark variant, as solarized-selenized-light is.
;;
;;; Code:

(require 'solarized)
(eval-when-compile
  (require 'solarized-palettes))

(deftheme solarized-flexoki-light
  "The Solarized theme with the Flexoki light palette.")

(solarized-with-color-variables 'dark 'solarized-flexoki-light
  solarized-flexoki-light-color-palette-alist)

(provide-theme 'solarized-flexoki-light)

(provide 'solarized-flexoki-light-theme)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
