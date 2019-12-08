;;; solarized-wombat-dark-theme.el --- Solarized dark theme with the wombat palette  -*- lexical-binding: t -*-

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
;; The the dark solarized theme with the wombat palette
;;
;;; Code:

(require 'solarized)
(eval-when-compile
  (require 'solarized-palettes))

(deftheme solarized-wombat-dark "The the dark solarized theme with the wombat palette")

(solarized-with-color-variables-with-palette 'dark 'solarized-wombat-dark
  '("#2a2a29" "#f6f3e8"           ; base03 (02 01 00 0 1 2) base3
    "#e5c06d" "#ddaa6f"           ; yellow orange
    "#ffb4ac" "#e5786d"           ; red    magenta
    "#834c98" "#a4b5e6"           ; violet blue
    "#7ec98f" "#8ac6f2"           ; cyan   green
    )
  '((custom-theme-set-faces
     theme-name
     `(default ((,class (:foreground ,(solarized-color-blend base03 base3 0.15 2) :background ,base03))))
     `(highlight ((,class (:background ,violet))))
     `(font-lock-builtin-face ((,class (:foreground ,magenta))))
     `(font-lock-constant-face ((,class (:foreground ,blue))))
     `(font-lock-comment-face ((,class (:foreground ,base00))))
     `(mode-line
       ((,class (:foreground ,base2 :background ,(solarized-color-blend base03 base3 0.85 2)))))
     `(mode-line-inactive
       ((,class (:foreground ,base00 :background ,(solarized-color-blend base03 "black" 0.85 2)))))
     `(mode-line-buffer-id ((,class (:foreground ,base3 :weight bold))))
     `(minibuffer-prompt ((,class (:foreground ,base1)))))))

(provide-theme 'solarized-wombat-dark)

(provide 'solarized-wombat-dark-theme)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; solarized-wombat-dark-theme.el ends here
