;;; solarized-config.el --- Solarized additional configurations for Emacs.

;; Copyright (C) 2011-2013
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; Author: Thomas Frössman <thomasf@jossystem.se>
;; URL: http://github.com/bbatsov/solarized-emacs
;; Version: 0.0.1

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
;; Additional configurations for various modes.
;;
;;; Installation:
;;
;;   Drop the `solarized-config.el` somewhere in your `load-path` and
;; the two themes in a folder that is on `custom-theme-load-path'
;; and enjoy!
;;
;;; Credits
;;
;; Ethan Schoonover created the original theme for vim on such this port
;; is based.
;;
;;; Code:


;; solarized-config-git-gutter
(defun solarized-config-git-gutter ()
  "TODO"
  (interactive)


  (eval-after-load "git-gutter"
    '(progn
       ;; (setq solarized-)
       (let ((symbol "▌"))
         (setq git-gutter:added-sign symbol
               git-gutter:modified-sign symbol
               ;; git-gutter:deleted-sign "⌞"
               git-gutter:deleted-sign symbol
               git-gutter:unchanged-sign nil
               git-gutter:window-width 1)))))

(defun solarized-config--smaller-font-hook  ()
  (text-scale-set -1))

;; (defcustom solarized-config-smaller-fonts-hooks
;;   '(
;;     flycheck-error-list-mode-hook
;;     magit-status-mode-hook
;;     magit-log-mode-hook
;;     magit-commit-mode-hook
;;     ag-mode-hook
;;     ))


;; solarized-config-smaller-fonts
(defun solarized-config-smaller-fonts-wip ()
  "DOCSTRING"
  (interactive)

  )


;; solarized-config-uniquify

;; solarized-config-ibuffer



;; solarized-config-flycheck
(dont-compile
  ;; TODO finish flycheck config
  (require 'fringe-helper)
  (fringe-helper-define 'vertical-wave-bitmap '(center repeat)
                        "...XXX."
                        "...XXX."
                        "..XXX.."
                        "..XXX..")

  (flycheck-define-error-level 'error
                               :overlay-category 'flycheck-error-overlay
                               :fringe-bitmap 'vertical-wave-bitmap
                               :fringe-face 'flycheck-fringe-error)

  (flycheck-define-error-level 'warning
                               :overlay-category 'flycheck-warning-overlay
                               :fringe-bitmap 'vertical-wave-bitmap
                               :fringe-face 'flycheck-fringe-warning)

  (flycheck-define-error-level 'info
                               :overlay-category 'flycheck-info-overlay
                               :fringe-bitmap 'vertical-wave-bitmap
                               :fringe-face 'flycheck-fringe-info))

;; solarized-config-









(provide 'solarized-config)

;;; solarized-config.el ends here
