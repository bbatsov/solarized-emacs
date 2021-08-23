;;; solarized-theme.el --- The Solarized color theme  -*- lexical-binding: t -*-

;; Copyright (C) 2011-2021 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; Author: Thomas Frössman <thomasf@jossystem.se>
;; URL: http://github.com/bbatsov/solarized-emacs
;; Version: 1.3.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: convenience, themes, solarized

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
;; A port of Solarized to Emacs.
;;
;;; Installation:
;;
;;   Drop the `solarized-theme.el` somewhere in your `load-path` and
;; the two themes in a folder that is on `custom-theme-load-path'
;; and enjoy!
;;
;; Don't forget that the theme requires Emacs 24.
;;
;;; Credits
;;
;; Ethan Schoonover created the original theme for vim on such this port
;; is based.
;;
;;; Code:

(require 'solarized)

(provide 'solarized-theme)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; solarized-theme.el ends here
