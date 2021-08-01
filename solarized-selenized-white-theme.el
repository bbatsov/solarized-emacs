;;; solarized-selenized-white-theme.el --- Solarized Theme  -*- lexical-binding: t -*-

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
;; The white variant of the solarized theme.
;;
;;; Code:

(require 'solarized)
(eval-when-compile
  (require 'solarized-palettes))

(deftheme solarized-selenized-white
  "The white variant of the Solarized colour theme with selenized color palette")

(solarized-with-color-variables 'dark 'solarized-selenized-white
  solarized-selenized-white-color-palette-alist)

(provide-theme 'solarized-selenized-white)

(provide 'solarized-selenized-white-theme)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; solarized-selenized-white-theme.el ends here
