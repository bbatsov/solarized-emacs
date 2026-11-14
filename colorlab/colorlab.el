;;; colorlab.el --- Solarized color palette generator -*- lexical-binding: t -*-

;; Copyright (C) 2011-2025 Bozhidar Batsov
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; Author: Thomas Frössman <thomasf@jossystem.se>

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

;; This utility generates color palette lists for use in themes and updates
;; solarized-palettes.el.
;;
;; It translates color palette generation into Elisp, using Oklab color
;; space blending and lightness adjustments to compute the accent pairs and
;; light/dark variants.
;;
;; Colorlab requires Emacs 30.1 or later, for the Oklab functions in
;; color.el.
;;
;; Usage:
;;   Interactive:
;;     M-x colorlab-update-palettes
;;
;;   Batch / Command line:
;;     emacs -Q --batch -l colorlab/colorlab.el
;;     emacs -Q --batch -l colorlab/colorlab.el -f colorlab-main -- --no-update
;;     emacs -Q --batch -l colorlab/colorlab.el -f colorlab-main -- --palette solarized-dark

;;; Code:

(require 'cl-lib)

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name default-directory)))
(require 'colorlab-engine)
(require 'colorlab-palettes)

(declare-function rainbow-mode "rainbow-mode" (&optional arg))

;;; Accent Pair Presets

;; The Oklab parameters below are fitted to reproduce the intensity of the
;; earlier L*a*b* generation.

(defconst colorlab-default-accent1
  '(:bg-amt 0.855 :fg-amt 0.305 :gamma 0.01 :min-diff 0.33 :fg-finder extreme-fg :bg-finder extreme-bg)
  "Default parameters for the subtle accent pair (NAME-1bg and NAME-1fg).")

(defconst colorlab-default-accent2
  '(:bg-amt 0.595 :fg-amt 0.46 :gamma 0.04 :min-diff 0.325 :fg-finder extreme-fg :bg-finder extreme-bg)
  "Default parameters for the highlight accent pair (NAME-2bg and NAME-2fg).")

(defconst colorlab-muted-light-accent1
  '(:bg-amt 0.855 :fg-amt 0.295 :gamma 0.007 :min-diff 0.30 :fg-finder extreme-fg :bg-finder extreme-bg)
  "Subtle accent pair parameters for low contrast light palettes.")

(defconst colorlab-muted-light-accent2
  '(:bg-amt 0.58 :fg-amt 0.42 :gamma 0.04 :min-diff 0.32 :fg-finder extreme-fg :bg-finder extreme-bg)
  "Highlight accent pair parameters for low contrast light palettes.")

(defconst colorlab-solarized-accent1
  '(:bg-amt 0.80 :fg-amt 0.25 :gamma 0.02 :min-diff 0.39 :fg-finder "base1" :bg-finder extreme-bg)
  "Subtle accent pair parameters for the Solarized dark palettes.")

(defconst colorlab-solarized-accent2
  '(:bg-amt 0.57 :fg-amt 0.40 :gamma 0.06 :min-diff 0.41 :fg-finder "base1" :bg-finder extreme-bg)
  "Highlight accent pair parameters for the Solarized dark palettes.")

;;; Palette Configurations

(defvar colorlab-palettes
  (list
   (list :name "solarized-dark"
         :solarized colorlab-solarized
         :accent1 colorlab-solarized-accent1
         :accent2 colorlab-solarized-accent2)
   (list :name "solarized-light"
         :solarized colorlab-solarized
         :inverse t
         :accent1 colorlab-muted-light-accent1
         :accent2 colorlab-muted-light-accent2)
   (list :name "solarized-dark-high-contrast"
         :solarized colorlab-solarized-dark-high-contrast
         :accent1 colorlab-solarized-accent1
         :accent2 colorlab-solarized-accent2)
   (list :name "solarized-light-high-contrast"
         :solarized colorlab-solarized-light-high-contrast
         :inverse t
         :accent1 colorlab-muted-light-accent1
         :accent2 colorlab-muted-light-accent2)
   (list :name "gruvbox-dark"
         :solarized colorlab-gruvbox-dark)
   (list :name "gruvbox-light"
         :solarized colorlab-gruvbox-light
         :inverse t)
   (list :name "zenburn"
         :solarized colorlab-zenburn)
   (list :name "monokai"
         :solarized colorlab-monokai)
   (list :name "selenized-dark"
         :solarized colorlab-selenized-dark
         :accent1 '(:bg-amt 0.80 :fg-amt 0.22 :gamma 0.02 :min-diff 0.46 :fg-finder extreme-fg :bg-finder extreme-bg)
         :accent2 '(:bg-amt 0.55 :fg-amt 0.40 :gamma 0.06 :min-diff 0.40 :fg-finder extreme-fg :bg-finder extreme-bg))
   (list :name "selenized-black"
         :solarized colorlab-selenized-black)
   (list :name "selenized-light"
         :solarized colorlab-selenized-light
         :inverse t
         :accent1 colorlab-muted-light-accent1
         :accent2 colorlab-muted-light-accent2)
   (list :name "selenized-white"
         :solarized colorlab-selenized-white
         :inverse t)
   (list :name "everforest-dark"
         :solarized colorlab-everforest-dark)
   (list :name "everforest-light"
         :solarized colorlab-everforest-light
         :inverse t
         ;; The cream page sits close to yellow, so the backgrounds carry their
         ;; accent hue (bg-tint) to stand out without getting darker, which
         ;; would cost syntax colors drawn on top of them their contrast.
         :accent1 '(:bg-amt 0.76 :fg-amt 0.295 :gamma 0.0 :min-diff 0.39 :bg-tint 2.5 :fg-finder extreme-fg :bg-finder extreme-bg)
         :accent2 '(:bg-amt 0.74 :fg-amt 0.42 :gamma 0.04 :min-diff 0.33 :bg-tint 3.0 :fg-finder extreme-fg :bg-finder extreme-bg))
   (list :name "flexoki-dark"
         :solarized colorlab-flexoki-dark)
   (list :name "flexoki-light"
         :solarized colorlab-flexoki-light
         :inverse t)
   (list :name "kanagawa-wave"
         :solarized colorlab-kanagawa-wave)
   (list :name "kanagawa-dragon"
         :solarized colorlab-kanagawa-dragon)
   (list :name "kanagawa-lotus"
         :solarized colorlab-kanagawa-lotus
         :inverse t
         ;; Yellow shares the hue of the page, so its backgrounds only separate
         ;; by lightness; stronger blends keep yellow-1bg clear of base2.  The
         ;; highlight pair adds accent chroma (bg-tint) instead of darkness, so
         ;; syntax colors drawn on top of it keep their contrast.
         :accent1 '(:bg-amt 0.78 :fg-amt 0.295 :gamma 0.0 :min-diff 0.30 :fg-finder extreme-fg :bg-finder extreme-bg)
         :accent2 '(:bg-amt 0.64 :fg-amt 0.42 :gamma 0.02 :min-diff 0.30 :bg-tint 2.0 :fg-finder extreme-fg :bg-finder extreme-bg))
   (list :name "nightfox"
         :solarized colorlab-nightfox)
   (list :name "dayfox"
         :solarized colorlab-dayfox
         :inverse t)
   (list :name "dawnfox"
         :solarized colorlab-dawnfox
         :inverse t
         :accent1 colorlab-muted-light-accent1
         :accent2 colorlab-muted-light-accent2)
   (list :name "duskfox"
         :solarized colorlab-duskfox)
   (list :name "nordfox"
         :solarized colorlab-nordfox)
   (list :name "terafox"
         :solarized colorlab-terafox)
   (list :name "carbonfox"
         :solarized colorlab-carbonfox)
   (list :name "carbon-dark"
         :solarized colorlab-carbon-dark)
   (list :name "carbon-light"
         :solarized colorlab-carbon-light
         :inverse t)
   (list :name "apple-dark"
         :solarized colorlab-apple-dark)
   (list :name "apple-light"
         :solarized colorlab-apple-light
         :inverse t
         ;; The foreground extreme is pure black, which pulls blends harder in
         ;; Oklab than it did in L*a*b*.
         :accent1 '(:bg-amt 0.81 :fg-amt 0.26 :gamma 0.03 :min-diff 0.31 :fg-finder extreme-fg :bg-finder extreme-bg)
         :accent2 '(:bg-amt 0.545 :fg-amt 0.39 :gamma 0.06 :min-diff 0.35 :fg-finder extreme-fg :bg-finder extreme-bg)))
  "List of all palette configurations.")

;;; Palette File Rewriter

(defun colorlab-find-palettes-file (&optional file)
  "Locate the solarized-palettes.el file."
  (or file
      (let* ((dir (file-name-directory (or load-file-name buffer-file-name default-directory)))
             (parent-rel (expand-file-name "../solarized-palettes.el" dir))
             (here-rel (expand-file-name "solarized-palettes.el" dir))
             (cwd-rel (expand-file-name "solarized-palettes.el" default-directory)))
        (cond
         ((file-exists-p parent-rel) parent-rel)
         ((file-exists-p here-rel) here-rel)
         ((file-exists-p cwd-rel) cwd-rel)
         (t parent-rel)))))

(defun colorlab-generate-palette (palette-name)
  "Generate and display the color alist for PALETTE-NAME."
  (interactive
   (list (completing-read "Palette: " (mapcar (lambda (p) (plist-get p :name)) colorlab-palettes))))
  (let* ((pal (cl-find palette-name colorlab-palettes :key (lambda (p) (plist-get p :name)) :test #'string=)))
    (unless pal
      (error "Palette %s not found" palette-name))
    (let ((nc (colorlab-generate-palette-alist pal)))
      (if (called-interactively-p 'any)
          (let ((buf (get-buffer-create (format "*colorlab-%s*" palette-name))))
            (with-current-buffer buf
              (erase-buffer)
              (insert (format ";; %s palette\n" palette-name))
              (insert (colorlab-format-alist nc 0))
              (emacs-lisp-mode)
              (when (or (fboundp 'rainbow-mode)
                        (require 'rainbow-mode nil t))
                (rainbow-mode 1)))
            (pop-to-buffer buf))
        nc))))

(defun colorlab--replace-palette-block (name nc)
  "Replace the entries of palette NAME in the current buffer with NC.
The entries sit between a \";; NAME palette\" line and a
\";; palette end\" line."
  (goto-char (point-min))
  (when (re-search-forward
         (format ";;[ \t]*%s\\(?:[ \t]+color\\)?[ \t]+palette[ \t]*$" (regexp-quote name))
         nil t)
    (forward-line 1)
    (let ((start (point)))
      (if (re-search-forward "^[ \t]*;;[ \t]*palette end[ \t]*$" nil t)
          (progn
            (beginning-of-line)
            (delete-region start (point))
            (insert (colorlab-format-alist nc 4)))
        (message "Warning: palette end marker not found for %s" name)))))

(defun colorlab-update-palettes (&optional file no-update palette-filter)
  "Generate palette entries and update solarized-palettes.el.
FILE is the target file, defaulting to solarized-palettes.el.
If NO-UPDATE is non-nil, don't modify the file.
If PALETTE-FILTER is given, only process matching palette names."
  (interactive)
  (let* ((target-file (colorlab-find-palettes-file file))
         (palettes (if palette-filter
                       (cl-remove-if-not (lambda (p) (string= (plist-get p :name) palette-filter)) colorlab-palettes)
                     colorlab-palettes)))
    (when (null palettes)
      (error "No palettes matched filter: %s" palette-filter))
    (unless (or no-update (file-exists-p target-file))
      (error "Target palette file not found: %s" target-file))
    (with-temp-buffer
      (unless no-update
        (insert-file-contents target-file))
      (dolist (pal palettes)
        (let ((name (plist-get pal :name))
              (nc (colorlab-generate-palette-alist pal)))
          (princ (format "%s\n" name))
          (princ (colorlab-format-alist nc 0))
          (unless no-update
            (colorlab--replace-palette-block name nc))))
      (princ "\n-----\n\n")
      (unless no-update
        (write-region (point-min) (point-max) target-file nil 'quiet)
        (message "Updated %s" target-file)))))

(defun colorlab-main ()
  "Command-line entry point for colorlab."
  (let ((no-update nil)
        (target-file nil)
        (palette-name nil)
        (args command-line-args-left))
    (while args
      (let ((arg (car args)))
        (cond
         ((string= arg "--no-update")
          (setq no-update t))
         ((string= arg "--file")
          (setq args (cdr args))
          (setq target-file (car args)))
         ((string= arg "--palette")
          (setq args (cdr args))
          (setq palette-name (car args)))
         ((member arg '("-h" "--help"))
          (princ "Usage: colorlab [OPTIONS]
Options:
  --no-update     Do not write to solarized-palettes.el
  --file FILE     Specify destination palettes file
  --palette NAME  Generate only the specified palette
  -h, --help      Show this help message
")
          (kill-emacs 0))))
      (setq args (cdr args)))
    (setq command-line-args-left nil)
    (colorlab-update-palettes target-file no-update palette-name)))

;; Auto-run colorlab-main when invoked as a script or loaded directly via -l in batch mode
(when (and noninteractive
           (not (bound-and-true-p eldev-project))
           (not (member "-f" command-line-args))
           (not (member "--eval" command-line-args))
           (not (member "--execute" command-line-args))
           (let ((target (or (cadr (member "-l" command-line-args))
                             (cadr (member "--script" command-line-args))
                             (car (last command-line-args)))))
             (and target (string-match-p "colorlab\\.el\\'" target))))
  (colorlab-main))

(provide 'colorlab)
;;; colorlab.el ends here
