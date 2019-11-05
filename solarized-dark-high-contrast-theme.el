;;; solarized-dark-high-contrast-theme.el --- Solarized Light Theme for Emacs.

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

(defvar solarized-dark-high-contrast-palette-alist
  (copy-alist solarized-color-palette-alist))

(let ((p solarized-dark-high-contrast-palette-alist)
      (o solarized-color-palette-alist)
      (a 0.9)
      (a2 0.85)
      (white "#ffffe8")
      (black "#051018"))
  (setcdr (assq 'base03 p) (solarized-color-blend (alist-get 'base03 o) black a  2))
  (setcdr (assq 'base02 p) (solarized-color-blend (alist-get 'base02 o) black a  2))
  (setcdr (assq 'base01 p) (solarized-color-blend (alist-get 'base01 o) white a  2))
  (setcdr (assq 'base00 p) (solarized-color-blend (alist-get 'base00 o) white a2 2))
  (setcdr (assq 'base0  p) (solarized-color-blend (alist-get 'base0  o) white a2 2))
  (setcdr (assq 'base1  p) (solarized-color-blend (alist-get 'base1  o) white a2 2))
  (setcdr (assq 'base2  p) (solarized-color-blend (alist-get 'base2  o) black a  2))
  (setcdr (assq 'base3  p) (solarized-color-blend (alist-get 'base3  o) black a  2)))

(deftheme solarized-dark-high-contrast
  "The dark variant of the Solarized colour theme with slightly higher text contrast")

(solarized-definition 'dark 'solarized-dark-high-contrast
                      solarized-dark-high-contrast-palette-alist)

(provide-theme 'solarized-dark-high-contrast)


(provide 'solarized-dark-high-contrast-theme)
;;; solarized-dark-high-contrast-theme.el ends here
