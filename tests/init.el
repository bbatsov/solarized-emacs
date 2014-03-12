(defconst tests-directory
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(setq
 package-user-dir (expand-file-name "elpa" tests-directory)
 inhibit-startup-message t
 inhibit-splash-screen t
 inhibit-startup-buffer-menu t
 inhibit-startup-echo-area-message t
 initial-scratch-message ";;_
;;                 __         _,******
;;   ,------,        _  _,**
;;   | Moo! |          _   ____,****
;;   ;------;        _
;;        \\   ^__^
;;         \\  (^^)\\_______
;;          ^-(..)\\       )\\/\\/^_^
;;                ||----w |
;; __.-''*-,.,____||_____||___,_.-
;;                 ''     ''

")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(let ((deps '(web-mode
              js2-mode
              haskell-mode
              undo-tree
              dash
              s
              f))
      (refreshed nil))
  (dolist (pkg deps)
    (when (not (package-installed-p pkg))
      (when (not refreshed)
        (package-refresh-contents)
        (setq refreshed t))
      (package-install pkg))))

(defconst solarized-directory
  (expand-file-name "../" tests-directory))
(setq load-path (cons solarized-directory load-path))
(if (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path solarized-directory))

(tool-bar-mode -1)
(menu-bar-mode -1)
