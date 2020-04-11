;;; solarized.el --- Solarized theme  -*- lexical-binding: t -*-

;; Copyright (C) 2011-2019 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
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

;; Main solarized file

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'solarized-faces)

;;; Options

(defgroup solarized nil
  "Solarized theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom solarized-theme-dir (locate-user-emacs-file "themes/")
  "Directory to save theme file."
  :type 'directory
  :group 'solarized)

(defcustom solarized-distinct-fringe-background nil
  "Make the fringe background different from the normal background color.
Also affects `linum-mode' background."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-distinct-doc-face nil
  "Make `font-lock-doc-face' stand out more.
Related discussion: https://github.com/bbatsov/solarized-emacs/issues/158"
  :type 'boolean
  :group 'solarized)

(defcustom solarized-use-variable-pitch t
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-use-less-bold nil
  "Use bold weight less often."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-use-more-italic nil
  "Use italic slant more often."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-emphasize-indicators t
  "Use more colors for indicators such as git:gutter, flycheck and similar."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-high-contrast-mode-line nil
  "Make the active/inactive mode line stand out more."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'solarized)

(defcustom solarized-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'solarized)

(defcustom solarized-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'solarized)

(defcustom solarized-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'solarized)

(defcustom solarized-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'solarized)

(defcustom solarized-scale-org-headlines t
  "Whether `org-mode' headlines should be scaled."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-scale-outline-headlines t
  "Whether `outline-mode' headlines should be scaled."
  :type 'boolean
  :group 'solarized)

;;; Utilities

(defun solarized-color-clamp-lab (lab)
  "Restricts a LAB colorspace color if it is out of bounds."
  (list (min (max (nth 0 lab) 0.0) 100.0)
        (min (max (nth 1 lab) -128) 127)
        (min (max (nth 2 lab) -128) 127)))

;;;###autoload
(defun solarized-color-blend (color1 color2 alpha &optional digits-per-component)
  "Blends COLOR1 onto COLOR2 with ALPHA.

COLOR1 and COLOR2 should be color names (e.g. \"white\") or RGB
triplet strings (e.g. \"#ff12ec\").

Alpha should be a float between 0 and 1.

Optional argument DIGITS-PER-COMPONENT can be either 4 (the default) or 2;
use the latter if you need a 24-bit specification of a color."
  (let ((args (mapcar 'color-clamp
                      (apply 'color-lab-to-srgb
                             (solarized-color-clamp-lab
                              (cl-mapcar
                               (lambda (v1 v2) (+ v1 (* alpha (- v2 v1))))
                               (apply 'color-srgb-to-lab (color-name-to-rgb color2))
                               (apply 'color-srgb-to-lab (color-name-to-rgb color1))))))))
    (if (version< emacs-version "26")
        (apply 'color-rgb-to-hex `(,@args))
      (apply 'color-rgb-to-hex `(,@args ,digits-per-component)))))

;;;###autoload
(defun solarized-create-color-palette (core-palette)
  "Create color-palette from CORE-PALETTE.

The Returned color-palette has the same format as `solarized-color-palette'"
  (let ((darkest-base   (nth 0 core-palette))
        (brightest-base (nth 1 core-palette))
        (yellow         (nth 2 core-palette))
        (orange         (nth 3 core-palette))
        (red            (nth 4 core-palette))
        (magenta        (nth 5 core-palette))
        (violet         (nth 6 core-palette))
        (blue           (nth 7 core-palette))
        (cyan           (nth 8 core-palette))
        (green          (nth 9 core-palette)))

    `((base03  . ,(solarized-color-blend darkest-base brightest-base 1.00 2))
      (base02  . ,(solarized-color-blend darkest-base brightest-base 0.97 2))
      (base01  . ,(solarized-color-blend darkest-base brightest-base 0.65 2))
      (base00  . ,(solarized-color-blend darkest-base brightest-base 0.60 2))
      (base0   . ,(solarized-color-blend darkest-base brightest-base 0.48 2))
      (base1   . ,(solarized-color-blend darkest-base brightest-base 0.42 2))
      (base2   . ,(solarized-color-blend darkest-base brightest-base 0.06 2))
      (base3   . ,(solarized-color-blend darkest-base brightest-base 0.00 2))

      ;; Solarized accented colors
      (yellow    . ,yellow)
      (orange    . ,orange)
      (red       . ,red)
      (magenta   . ,magenta)
      (violet    . ,violet)
      (blue      . ,blue)
      (cyan      . ,cyan)
      (green     . ,green)

      ;; Darker and lighter accented colors
      ;; Only use these in exceptional circumstances!
      (yellow-d  . ,(solarized-color-blend darkest-base   yellow  0.80 2))
      (yellow-l  . ,(solarized-color-blend brightest-base yellow  0.80 2))
      (orange-d  . ,(solarized-color-blend darkest-base   orange  0.80 2))
      (orange-l  . ,(solarized-color-blend brightest-base orange  0.80 2))
      (red-d     . ,(solarized-color-blend darkest-base   red     0.80 2))
      (red-l     . ,(solarized-color-blend brightest-base red     0.80 2))
      (magenta-d . ,(solarized-color-blend darkest-base   magenta 0.80 2))
      (magenta-l . ,(solarized-color-blend brightest-base magenta 0.80 2))
      (violet-d  . ,(solarized-color-blend darkest-base   violet  0.80 2))
      (violet-l  . ,(solarized-color-blend brightest-base violet  0.80 2))
      (blue-d    . ,(solarized-color-blend darkest-base   blue    0.80 2))
      (blue-l    . ,(solarized-color-blend brightest-base blue    0.80 2))
      (cyan-d    . ,(solarized-color-blend darkest-base   cyan    0.80 2))
      (cyan-l    . ,(solarized-color-blend brightest-base cyan    0.80 2))
      (green-d   . ,(solarized-color-blend darkest-base   green   0.80 2))
      (green-l   . ,(solarized-color-blend brightest-base green   0.80 2))

      (yellow-1bg  . ,(solarized-color-blend darkest-base yellow  0.85 2))
      (orange-1bg  . ,(solarized-color-blend darkest-base orange  0.85 2))
      (red-1bg     . ,(solarized-color-blend darkest-base red     0.85 2))
      (magenta-1bg . ,(solarized-color-blend darkest-base magenta 0.85 2))
      (blue-1bg    . ,(solarized-color-blend darkest-base blue    0.85 2))
      (cyan-1bg    . ,(solarized-color-blend darkest-base cyan    0.85 2))
      (green-1bg   . ,(solarized-color-blend darkest-base green   0.85 2))
      (violet-1bg  . ,(solarized-color-blend darkest-base violet  0.85 2))

      (yellow-1fg  . ,(solarized-color-blend brightest-base yellow  0.30 2))
      (orange-1fg  . ,(solarized-color-blend brightest-base orange  0.30 2))
      (red-1fg     . ,(solarized-color-blend brightest-base red     0.30 2))
      (magenta-1fg . ,(solarized-color-blend brightest-base magenta 0.30 2))
      (violet-1fg  . ,(solarized-color-blend brightest-base violet  0.30 2))
      (blue-1fg    . ,(solarized-color-blend brightest-base blue    0.30 2))
      (cyan-1fg    . ,(solarized-color-blend brightest-base cyan    0.30 2))
      (green-1fg   . ,(solarized-color-blend brightest-base green   0.30 2))

      (yellow-2bg  . ,(solarized-color-blend darkest-base yellow  0.60 2))
      (orange-2bg  . ,(solarized-color-blend darkest-base orange  0.60 2))
      (red-2bg     . ,(solarized-color-blend darkest-base red     0.60 2))
      (magenta-2bg . ,(solarized-color-blend darkest-base magenta 0.60 2))
      (violet-2bg  . ,(solarized-color-blend darkest-base violet  0.60 2))
      (blue-2bg    . ,(solarized-color-blend darkest-base blue    0.60 2))
      (cyan-2bg    . ,(solarized-color-blend darkest-base cyan    0.60 2))
      (green-2bg   . ,(solarized-color-blend darkest-base green   0.60 2))

      (yellow-2fg  . ,(solarized-color-blend brightest-base yellow  0.45 2))
      (orange-2fg  . ,(solarized-color-blend brightest-base orange  0.45 2))
      (red-2fg     . ,(solarized-color-blend brightest-base red     0.45 2))
      (magenta-2fg . ,(solarized-color-blend brightest-base magenta 0.45 2))
      (violet-2fg  . ,(solarized-color-blend brightest-base violet  0.45 2))
      (blue-2fg    . ,(solarized-color-blend brightest-base blue    0.45 2))
      (cyan-2fg    . ,(solarized-color-blend brightest-base cyan    0.45 2))
      (green-2fg   . ,(solarized-color-blend brightest-base green   0.45 2)))))

;;; Setup Start
(defmacro solarized-with-color-variables (variant theme-name color-palette &optional childtheme-sexp)
  "Eval `solarized-definition' in solarized COLOR-PALETTE for THEME-NAME.
VARIANT is 'dark or 'light.
When optional argument CHILDTHEME-SEXP sexp is supplied it's invoked to further
customize the resulting theme."
  (declare (indent defun))
  (let ((color-palette* (eval color-palette)))
    `(let* ((class '((class color) (min-colors 89)))
            (light-class (append '((background light)) class))
            (dark-class (append '((background dark)) class))
            (theme-name ,theme-name)
            (variant ,variant)
            ,@(mapcar (lambda (elm) `(,(car elm) ,(cdr elm))) color-palette*)

            (s-base03 base03)
            (s-base02 base02)
            (s-base01 base01)
            (s-base00 base00)
            (s-base3 base3)
            (s-base2 base2)
            (s-base1 base1)
            (s-base0 base0)

            ;; Solarized palette names, use these instead of -fg -bg...
            (base03 (if (eq variant 'light) s-base3 s-base03))
            (base02 (if (eq variant 'light) s-base2 s-base02))
            (base01 (if (eq variant 'light) s-base1 s-base01))
            (base00 (if (eq variant 'light) s-base0 s-base00))
            (base0 (if (eq variant 'light) s-base00 s-base0))
            (base1 (if (eq variant 'light) s-base01 s-base1))
            (base2 (if (eq variant 'light) s-base02 s-base2))
            (base3 (if (eq variant 'light) s-base03 s-base3))

            ;; Line drawing color
            ;;
            ;; NOTE only use this for very thin lines that are hard to see using base02, in low
            ;; color displayes base02 might be used instead
            (s-line (if (eq variant 'light) "#cccec4" "#284b54"))

            ;; Light/Dark adaptive higher/lower contrast accented colors
            ;;
            ;; NOTE Only use these in exceptional cirmumstances!
            (yellow-hc (if (eq variant 'light) yellow-d yellow-l))
            (yellow-lc (if (eq variant 'light) yellow-l yellow-d))
            (orange-hc (if (eq variant 'light) orange-d orange-l))
            (orange-lc (if (eq variant 'light) orange-l orange-d))
            (red-hc (if (eq variant 'light) red-d red-l))
            (red-lc (if (eq variant 'light) red-l red-d))
            (magenta-hc (if (eq variant 'light) magenta-d magenta-l))
            (magenta-lc (if (eq variant 'light) magenta-l magenta-d))
            (violet-hc (if (eq variant 'light) violet-d violet-l))
            (violet-lc (if (eq variant 'light) violet-l violet-d))
            (blue-hc (if (eq variant 'light) blue-d blue-l))
            (blue-lc (if (eq variant 'light) blue-l blue-d))
            (cyan-hc (if (eq variant 'light) cyan-d cyan-l))
            (cyan-lc (if (eq variant 'light) cyan-l cyan-d))
            (green-hc (if (eq variant 'light) green-d green-l))
            (green-lc (if (eq variant 'light) green-l green-d))

            ;; customize based face properties
            (s-maybe-bold (if solarized-use-less-bold
                              'unspecified 'bold))
            (s-maybe-italic (if solarized-use-more-italic
                                'italic 'normal))
            (s-variable-pitch (if solarized-use-variable-pitch
                                  'variable-pitch 'default))
            (s-fringe-bg (if solarized-distinct-fringe-background
                             base02 base03))
            (s-fringe-fg base01)

            (s-header-line-fg (if solarized-high-contrast-mode-line
                                  base1 base0))
            (s-header-line-bg (if solarized-high-contrast-mode-line
                                  base02 base03))
            (s-header-line-underline (if solarized-high-contrast-mode-line
                                         nil base02))

            (s-mode-line-fg (if solarized-high-contrast-mode-line
                                base03 base0))
            (s-mode-line-bg (if solarized-high-contrast-mode-line
                                base0 base02))
            (s-mode-line-underline (if solarized-high-contrast-mode-line
                                       nil s-line))

            (s-mode-line-buffer-id-fg (if solarized-high-contrast-mode-line
                                          'unspecified base1))
            (s-mode-line-inactive-fg (if solarized-high-contrast-mode-line
                                         base0 base01))
            (s-mode-line-inactive-bg (if solarized-high-contrast-mode-line
                                         base02 base03))
            (s-mode-line-inactive-bc (if solarized-high-contrast-mode-line
                                         base02 base02))

            ;; diff colors
            (s-diff-A-bg red-1bg)
            (s-diff-A-fg red-1fg)
            (s-diff-fine-A-bg red-2bg)
            (s-diff-fine-A-fg red-2fg)

            (s-diff-B-bg green-1bg)
            (s-diff-B-fg green-1fg)
            (s-diff-fine-B-bg green-2bg)
            (s-diff-fine-B-fg green-2fg)

            (s-diff-Ancestor-bg yellow-1bg)
            (s-diff-Ancestor-fg yellow-1fg)
            (s-diff-fine-Ancestor-bg yellow-2bg)
            (s-diff-fine-Ancestor-fg yellow-2fg)

            (s-diff-C-bg blue-1bg)
            (s-diff-C-fg blue-1fg)
            (s-diff-fine-C-bg blue-2bg)
            (s-diff-fine-C-fg blue-2fg)
            (s-diff-context-fg base0)
            (s-diff-heading-bg base02)

            (s-diffstat-added-fg green)
            (s-diffstat-changed-fg blue)
            (s-diffstat-removed-fg red))

       ;; NOTE: `custom--inhibit-theme-enable' turn-off needed
       ;;       childtheme works well disscussed in #352
       (let ((custom--inhibit-theme-enable nil))
         ,@solarized-definition
         ,@(eval childtheme-sexp)))))

(defmacro solarized-with-color-variables-with-palette (variant theme-name core-palette &optional childtheme-sexp)
  "Create a VARIANT of the theme named THEME-NAME with CORE-PALETTE.

When optional argument CHILDTHEME-SEXP sexp is supplied it's invoked to further
customize the resulting theme.

CORE-PALETTE is core color-palette."
  (declare (indent 2))
  (let ((color-palette (solarized-create-color-palette (eval core-palette))))
    `(solarized-with-color-variables ,variant ,theme-name ',color-palette ,childtheme-sexp)))

(defun solarized-create-theme-file (variant theme-name color-palette &optional childtheme-sexp overwrite)
  "Create a VARIANT of the theme named THEME-NAME with COLOR-PALETTE.

When optional argument CHILDTHEME-SEXP sexp is supplied it's invoked to further
customize the resulting theme.

CORE-PALETTE is core color-palette.
If OVERWRITE is non-nil, overwrite theme file if exist."
  (declare (indent 2))
  (add-to-list 'custom-theme-load-path solarized-theme-dir)
  (let ((path (expand-file-name (format "%s.el" theme-name)
                                solarized-theme-dir)))
    (unless (file-directory-p solarized-theme-dir)
      (make-directory solarized-theme-dir))
    (when (or overwrite (not (file-readable-p path)))
      (with-temp-file (expand-file-name (format "%s-theme.el" theme-name)
                                        solarized-theme-dir)
        (mapc (lambda (elm)
                (insert (pp-to-string elm)))
              `((require 'solarized)
                (deftheme ,theme-name
                  ,(format "The %s colour theme of Solarized colour theme flavor." theme-name))
                (solarized-with-color-variables ',variant ',theme-name ',color-palette ',childtheme-sexp)
                (provide-theme ',theme-name)
                (provide ',(intern (format "%s-theme" theme-name)))))))
    path))

(defun solarized-create-theme-file-with-palette (variant theme-name core-palette &optional childtheme-sexp overwrite)
  "Create a VARIANT of the theme named THEME-NAME with CORE-PALETTE.

When optional argument CHILDTHEME-SEXP sexp is supplied it's invoked to further
customize the resulting theme.

CORE-PALETTE is core color-palette.
If OVERWRITE is non-nil, overwrite theme file if exist."
  (declare (indent 2))
  (let ((color-palette (solarized-create-color-palette core-palette)))
    (apply #'solarized-create-theme-file (list variant theme-name color-palette childtheme-sexp overwrite))))

(define-obsolete-function-alias 'create-solarized-theme-file 'solarized-create-theme-file)

;;; Footer

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'solarized)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; solarized.el ends here
