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
(eval-when-compile
  (require 'solarized-palettes))

(deftheme solarized-gruvbox-dark
  "The dark variant of the Solarized colour theme with gruvbox color palette")

(solarized-with-color-variables 'dark 'solarized-gruvbox-dark
  solarized-gruvbox-dark-color-palette-alist)

(provide-theme 'solarized-gruvbox-dark)

(provide 'solarized-gruvbox-dark-theme)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; solarized-gruvbox-dark-theme.el ends here
