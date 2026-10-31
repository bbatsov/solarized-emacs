;;; solarized-apple-light-theme.el --- Solarized Theme  -*- lexical-binding: t -*-

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
;; The Solarized theme with the Apple light palette.
;;
;; The palette is from Apple's Human Interface Guidelines (2025),
;; https://developer.apple.com/design/human-interface-guidelines/color.
;;
;;; Code:

(require 'solarized)
(eval-when-compile
  (require 'solarized-palettes))

(deftheme solarized-apple-light
  "The Solarized theme with the Apple light palette.")

(solarized-with-color-variables 'dark 'solarized-apple-light
  solarized-apple-light-color-palette-alist)

(provide-theme 'solarized-apple-light)

(provide 'solarized-apple-light-theme)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
