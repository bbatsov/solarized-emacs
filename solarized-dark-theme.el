;;; solarized-dark-theme.el --- Solarized Dark Theme for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2011-2019 Bozhidar Batsov

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

(deftheme solarized-dark "The dark variant of the Solarized colour theme")

(solarized-with-color-variables 'dark 'solarized-dark
  solarized-dark-color-palette-alist)

(provide-theme 'solarized-dark)

(provide 'solarized-dark-theme)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; solarized-dark-theme.el ends here
