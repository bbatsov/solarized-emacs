;;; solarized-light-high-contrast-theme.el --- Solarized Light Theme for Emacs.

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

(defvar solarized-light-high-contrast-palette-alist
  (copy-alist solarized-color-palette-alist))

(let ((p solarized-light-high-contrast-palette-alist)
      (o solarized-color-palette-alist)
      (a 0.9)
      (a2 0.8)
      (a3 0.7)
      (white "#ffffe8")
      (black "#051018")
      (grey "#a0a0a0"))
  (setcdr (assq 's-base03 p) (solarized-color-blend (alist-get 's-base03 o) black a  2))
  (setcdr (assq 's-base02 p) (solarized-color-blend (alist-get 's-base02 o) black a  2))
  (setcdr (assq 's-base01 p) (solarized-color-blend (alist-get 's-base01 o) grey  a3 2))
  (setcdr (assq 's-base00 p) (solarized-color-blend (alist-get 's-base00 o) black a  2))
  (setcdr (assq 's-base0  p) (solarized-color-blend (alist-get 's-base0  o) black a2 2))
  (setcdr (assq 's-base1  p) (solarized-color-blend (alist-get 's-base1  o) black a3 2))
  (setcdr (assq 's-base2  p) (solarized-color-blend (alist-get 's-base2  o) white a  2))
  (setcdr (assq 's-base3  p) (solarized-color-blend (alist-get 's-base3  o) white a  2)))

(deftheme solarized-light-high-contrast
  "The light variant of the Solarized colour theme with slightly higher text contrast")

(solarized-definition 'light 'solarized-light-high-contrast
                      solarized-light-high-contrast-palette-alist)

(provide-theme 'solarized-light-high-contrast)

(provide 'solarized-light-high-contrast-theme)
;;; solarized-light-high-contrast-theme.el ends here
