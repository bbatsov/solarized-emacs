;;; solarized.el --- Solarized theme

;; Copyright (C) 2011-2019 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; Author: Thomas Frössman <thomasf@jossystem.se>
;; URL: http://github.com/bbatsov/solarized-emacs
;; Version: 1.3.0
;; Package-Requires: ((emacs "24.1") (dash "2.16"))
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

(require 'dash)
(require 'color)
(require 'solarized-palettes)
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

;;;###autoload
(defun solarized-color-blend (color1 color2 alpha &optional digits-per-component)
  "Blends COLOR1 onto COLOR2 with ALPHA.

COLOR1 and COLOR2 should be color names (e.g. \"white\") or RGB
triplet strings (e.g. \"#ff12ec\").

Alpha should be a float between 0 and 1.

Optional argument DIGITS-PER-COMPONENT can be either 4 (the default) or 2;
use the latter if you need a 24-bit specification of a color."
  (apply 'color-rgb-to-hex
         `(,@(-zip-with '(lambda (it other)
                           (+ (* alpha it) (* other (- 1 alpha))))
                        (color-name-to-rgb color1)
                        (color-name-to-rgb color2))
           ,digits-per-component)))

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
      (yellow-d  . ,(solarized-color-blend yellow  darkest-base   0.80 2))
      (yellow-l  . ,(solarized-color-blend yellow  brightest-base 0.80 2))
      (orange-d  . ,(solarized-color-blend orange  darkest-base   0.80 2))
      (orange-l  . ,(solarized-color-blend orange  brightest-base 0.80 2))
      (red-d     . ,(solarized-color-blend red     darkest-base   0.80 2))
      (red-l     . ,(solarized-color-blend red     brightest-base 0.80 2))
      (magenta-d . ,(solarized-color-blend magenta darkest-base   0.80 2))
      (magenta-l . ,(solarized-color-blend magenta brightest-base 0.80 2))
      (violet-d  . ,(solarized-color-blend violet  darkest-base   0.80 2))
      (violet-l  . ,(solarized-color-blend violet  brightest-base 0.80 2))
      (blue-d    . ,(solarized-color-blend blue    darkest-base   0.80 2))
      (blue-l    . ,(solarized-color-blend blue    brightest-base 0.80 2))
      (cyan-d    . ,(solarized-color-blend cyan    darkest-base   0.80 2))
      (cyan-l    . ,(solarized-color-blend cyan    brightest-base 0.80 2))
      (green-d   . ,(solarized-color-blend green   darkest-base   0.80 2))
      (green-l   . ,(solarized-color-blend green   brightest-base 0.80 2)))))



;;; Setup Start
(defmacro solarized-with-color-variables (variant color-palette &rest body)
  "Eval BODY in solarized COLOR-PALETTE.
VARIANT is 'dark or 'light."
  (declare (indent defun))
  `(let* ((class '((class color) (min-colors 89)))
          (light-class (append '((background light)) class))
          (dark-class (append '((background dark)) class))
          (variant ,variant)
          ,@(mapcar (lambda (elm) `(,(car elm) ,(cdr elm))) color-palette)

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
          (s-diff-A-bg (if (eq variant 'light) "#ffdddd" "#553333"))
          (s-diff-A-fg (if (eq variant 'light) "#aa2222" "#ffdddd"))
          (s-diff-fine-A-bg (if (eq variant 'light)
                                (solarized-color-blend "#ffdddd" red 0.7)
                              (solarized-color-blend "#664444" red 0.7)))
          (s-diff-fine-A-fg (if (eq variant 'light) "#aa2222" "#eecccc"))

          (s-diff-B-bg (if (eq variant 'light) "#ddffdd" "#335533"))
          (s-diff-B-fg (if (eq variant 'light) "#22aa22" "#ddffdd"))
          (s-diff-fine-B-bg (if (eq variant 'light)
                                (solarized-color-blend "#ddffdd" green 0.7)
                              (solarized-color-blend "#446644" green 0.7)))
          (s-diff-fine-B-fg (if (eq variant 'light) "#336633" "#cceecc"))

          (s-diff-Ancestor-bg (if (eq variant 'light) "#ffffcc" "#555522"))
          (s-diff-Ancestor-fg (if (eq variant 'light) "#aaaa11" "#ffffcc"))
          (s-diff-fine-Ancestor-bg (if (eq variant 'light) "#eeeebb" "#666622"))
          (s-diff-fine-Ancestor-fg (if (eq variant 'light) "#aaaa11" "#eeeebb"))

          (s-diff-C-bg (if (eq variant 'light) "#d1e2f2" "#004d7b"))
          (s-diff-C-fg (if (eq variant 'light) "#004d7b" "#d1e2f2"))
          (s-diff-fine-C-bg (if (eq variant 'light)
                                (solarized-color-blend "#d6e5e5" blue 0.7)
                              (solarized-color-blend "#00547f" blue 0.7)))
          (s-diff-fine-C-fg (if (eq variant 'light) "#00547f" "#d6e5e5"))
          (s-diff-context-fg (if (eq variant 'light) base0 (solarized-color-blend base1 base2 0.6)))
          (s-diff-heading-bg (if (eq variant 'light) (solarized-color-blend yellow base03 0.1) base02))

          (s-diffstat-added-fg green)
          (s-diffstat-changed-fg blue)
          (s-diffstat-removed-fg red)
          )
     ,@body))

(defun solarized-create-theme-file (variant theme-name core-palette &optional childtheme overwrite)
  "Create a VARIANT of the theme named THEME-NAME with CORE-PALETTE.

When optional argument CHILDTHEME function is supplied it's invoked to further
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
                (let ((custom--inhibit-theme-enable nil))
                  (solarized-create-theme-with-palette ',variant ',theme-name ',core-palette ',childtheme))
                (provide-theme ',theme-name)
                (provide ',(intern (format "%s-theme" theme-name)))))))
    path))

(defun solarized-create-theme (variant theme-name &optional childtheme)
  "Create a VARIANT of the theme named THEME-NAME.

When optional argument CHILDTHEME function is supplied it's invoked to further
customize the resulting theme."
  (solarized-definition variant theme-name solarized-color-palette-alist childtheme))

(defun solarized-create-theme-with-palette (variant theme-name core-palette &optional childtheme)
  "Create a VARIANT of the theme named THEME-NAME with CORE-PALETTE.

When optional argument CHILDTHEME function is supplied it's invoked to further
customize the resulting theme.

CORE-PALETTE is core color-palette."
  (declare (indent 2))
  (let ((color-palette (solarized-create-color-palette core-palette)))
    (solarized-definition variant theme-name color-palette childtheme)))

(define-obsolete-function-alias 'create-solarized-theme-file         'solarized-create-theme-file)
(define-obsolete-function-alias 'create-solarized-theme              'solarized-create-theme)
(define-obsolete-function-alias 'create-solarized-theme-with-palette 'solarized-create-theme-with-palette)

;;; Footer

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'solarized)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; fill-column: 95
;; End:
;;; solarized.el ends here
