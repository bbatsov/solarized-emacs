;;; solarized-test.el --- Tests for solarized-theme  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Buttercup test suite for the Solarized theme family.
;;
;; Face assertions read directly from the `theme-face' property rather than
;; going through `face-attribute' - in batch mode, faces aren't recomputed to
;; reflect theme specs, so `face-attribute' would miss what the theme sets.
;; `theme-face' is the source of truth.

;;; Code:

(require 'buttercup)
(require 'solarized)
(require 'solarized-palettes)

;; Make the theme files loadable from the project root.
(defconst solarized-test--root
  (expand-file-name
   ".." (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "Project root, used to find the theme sources.")

(add-to-list 'custom-theme-load-path solarized-test--root)

(defconst solarized-test--variants
  '(solarized-dark
    solarized-light
    solarized-dark-high-contrast
    solarized-light-high-contrast
    solarized-gruvbox-dark
    solarized-gruvbox-light
    solarized-selenized-black
    solarized-selenized-dark
    solarized-selenized-light
    solarized-selenized-white
    solarized-wombat-dark
    solarized-zenburn)
  "Every variant the package ships.")

(defconst solarized-test--palettes
  '((solarized-dark                 . solarized-dark-color-palette-alist)
    (solarized-light                . solarized-light-color-palette-alist)
    (solarized-dark-high-contrast   . solarized-dark-high-contrast-palette-alist)
    (solarized-light-high-contrast  . solarized-light-high-contrast-palette-alist)
    (solarized-gruvbox-dark         . solarized-gruvbox-dark-color-palette-alist)
    (solarized-gruvbox-light        . solarized-gruvbox-light-color-palette-alist)
    (solarized-selenized-black      . solarized-selenized-black-color-palette-alist)
    (solarized-selenized-dark       . solarized-selenized-dark-color-palette-alist)
    (solarized-selenized-light      . solarized-selenized-light-color-palette-alist)
    (solarized-selenized-white      . solarized-selenized-white-color-palette-alist)
    (solarized-zenburn              . solarized-zenburn-color-palette-alist))
  "Variants that draw on a named palette, and the variable holding it.
`solarized-wombat-dark' is absent on purpose: it passes its colors inline
to `solarized-with-color-variables-with-palette' rather than naming an
alist, so there is nothing here to point at.  Note also that the two
high-contrast variables break the `-color-palette-alist' naming the
others use.")

(defun solarized-test--reload (variant)
  "Disable any active Solarized theme and (re-)load VARIANT."
  (dolist (v solarized-test--variants)
    (when (custom-theme-enabled-p v)
      (disable-theme v))
    (put v 'theme-settings nil)
    (setq custom-known-themes (delq v custom-known-themes)))
  (load-theme variant t))

(defun solarized-test--face-attr (face variant attr)
  "Return ATTR from FACE's theme-face spec for VARIANT, or nil."
  (let* ((theme-face (get face 'theme-face))
         (entry      (assoc variant theme-face))
         (specs      (cadr entry))
         (first      (car specs))
         (props      (cadr first)))
    (plist-get props attr)))

(defun solarized-test--hex-p (color)
  "Return non-nil if COLOR is a six-digit hex string."
  (and (stringp color) (string-match-p "\\`#[0-9a-fA-F]\\{6\\}\\'" color)))

(defun solarized-test--luminance (hex)
  "Return the WCAG relative luminance of the color HEX."
  (let ((channels (mapcar
                   (lambda (offset)
                     (let ((v (/ (string-to-number
                                  (substring hex offset (+ offset 2)) 16)
                                 255.0)))
                       (if (<= v 0.04045) (/ v 12.92)
                         (expt (/ (+ v 0.055) 1.055) 2.4))))
                   '(1 3 5))))
    (+ (* 0.2126 (nth 0 channels))
       (* 0.7152 (nth 1 channels))
       (* 0.0722 (nth 2 channels)))))

(defun solarized-test--contrast (a b)
  "Return the WCAG contrast ratio between the colors A and B."
  (let* ((la (solarized-test--luminance a))
         (lb (solarized-test--luminance b))
         (lighter (max la lb)) (darker (min la lb)))
    (/ (+ lighter 0.05) (+ darker 0.05))))

(defun solarized-test--file-text (name)
  "Return the text of NAME, relative to the project root."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name solarized-test--root))
    (buffer-string)))

;;; Theme loading

(describe "theme loading"
  (after-each
    (dolist (v solarized-test--variants)
      (when (custom-theme-enabled-p v)
        (disable-theme v))))

  (dolist (variant solarized-test--variants)
    (it (format "loads %s and gives it a background" variant)
      (expect (solarized-test--reload variant) :to-be-truthy)
      (expect (custom-theme-enabled-p variant) :to-be-truthy)
      (expect (solarized-test--hex-p
               (solarized-test--face-attr 'default variant :background))
              :to-be-truthy))))

;;; Palette integrity

(describe "color palettes"
  (it "define the same keys in every named palette"
    (let ((reference (sort (mapcar #'car (symbol-value
                                          (cdr (car solarized-test--palettes))))
                           #'string<)))
      (dolist (entry solarized-test--palettes)
        (expect (cons (car entry)
                      (sort (mapcar #'car (symbol-value (cdr entry))) #'string<))
                :to-equal (cons (car entry) reference)))))

  (it "hold hex color values"
    (dolist (entry solarized-test--palettes)
      (dolist (pair (symbol-value (cdr entry)))
        (expect (solarized-test--hex-p (cdr pair)) :to-be-truthy)))))

;;; Syntax highlighting has to be readable on the background
;;
;; This is the contrast that matters most and is measured least: whether the
;; code you are looking at reads against the buffer behind it.  Comments sit
;; lowest by design, so the floor is set just under them rather than at a
;; WCAG grade the palette was never aiming for.

(defconst solarized-test--syntax-floor 2.4
  "Contrast the core syntax faces must reach against their own background.")

(defconst solarized-test--core-syntax-faces
  '(font-lock-comment-face font-lock-string-face font-lock-keyword-face
    font-lock-function-name-face font-lock-variable-name-face
    font-lock-type-face font-lock-constant-face font-lock-builtin-face
    font-lock-doc-face minibuffer-prompt link error warning success)
  "The faces a user reads all day, whatever else the theme covers.")

(describe "core syntax contrast"
  (after-each
    (dolist (v solarized-test--variants)
      (when (custom-theme-enabled-p v)
        (disable-theme v))))

  (dolist (variant solarized-test--variants)
    (it (format "keeps the core faces readable in %s" variant)
      (solarized-test--reload variant)
      (let ((bg (solarized-test--face-attr 'default variant :background))
            (illegible '()))
        (dolist (face solarized-test--core-syntax-faces)
          (let ((fg (solarized-test--face-attr face variant :foreground)))
            (when (and (solarized-test--hex-p fg)
                       (< (solarized-test--contrast fg bg)
                          solarized-test--syntax-floor))
              (push (list face (solarized-test--contrast fg bg)) illegible))))
        (expect illegible :to-equal '())))))

;;; Faces that inherit have to inherit something readable

(describe "tabbar"
  (after-each
    (dolist (v solarized-test--variants)
      (when (custom-theme-enabled-p v)
        (disable-theme v))))

  ;; `tabbar-unselected' takes its foreground from `tabbar-default' and only
  ;; overrides the background, so a foreground chosen to match
  ;; `tabbar-default''s own background left every unselected tab label
  ;; unreadable.
  (dolist (variant solarized-test--variants)
    (it (format "gives unselected tabs a readable label in %s" variant)
      (solarized-test--reload variant)
      (let ((fg (solarized-test--face-attr 'tabbar-default variant :foreground))
            (bg (solarized-test--face-attr 'tabbar-unselected variant :background)))
        (expect (solarized-test--contrast fg bg) :to-be-greater-than 3.0)))))

;;; The shape of the source

(describe "the source"
  (it "defines each face exactly once"
    (let* ((body (solarized-test--file-text "solarized-faces.el"))
           (start 0) (faces '()) (seen (make-hash-table :test 'equal)) (dupes '()))
      (while (string-match "`(\\([^ ()]+\\) ((,class" body start)
        (push (match-string 1 body) faces)
        (setq start (match-end 0)))
      (dolist (face (nreverse faces))
        (when (gethash face seen) (push face dupes))
        (puthash face t seen))
      (expect (delete-dups dupes) :to-equal '()))))

;;; Package headers

(describe "package headers"
  (it "opens with a summary and a lexical-binding cookie"
    (expect (car (split-string (solarized-test--file-text "solarized-theme.el") "\n"))
            :to-match (rx-to-string '(seq ";;; " (1+ nonl) " --- " (1+ nonl)
                                          "-*- lexical-binding: t" (opt ";") " -*-"))))

  (it "declares the headers a package needs"
    (let ((text (solarized-test--file-text "solarized-theme.el")))
      (dolist (header '("Author" "URL" "Version" "Package-Requires"))
        (expect (string-match-p (concat "^;; " header ": ") text) :not :to-be nil))))

  (it "declares a Package-Requires that reads back as an alist"
    (let* ((text (solarized-test--file-text "solarized-theme.el"))
           (_ (string-match "^;; Package-Requires: \\(.*\\)$" text))
           (deps (car (read-from-string (match-string 1 text)))))
      (expect (assq 'emacs deps) :not :to-be nil))))

;;; Emphasis restraint

(defconst solarized-test--emphatic-faces
  '(;; a boxed mode line with weight is the convention, not excess
    mode-line mode-line-inactive
    ;; the agenda leans on weight, slant and underline to separate the day
    ;; you are on from the ones around it
    org-agenda-date org-agenda-date-today org-agenda-structure)
  "Faces allowed to stack three emphasis attributes.")

(describe "emphasis"
  (after-each
    (dolist (v solarized-test--variants)
      (when (custom-theme-enabled-p v)
        (disable-theme v))))

  (it "rarely stacks three emphasis attributes on one face"
    (solarized-test--reload 'solarized-dark)
    (let ((overwrought '()))
      (mapatoms
       (lambda (sym)
         (when (assoc 'solarized-dark (get sym 'theme-face))
           (when (> (seq-count
                     (lambda (attr) (solarized-test--face-attr sym 'solarized-dark attr))
                     '(:weight :slant :underline :box :overline :strike-through))
                    2)
             (unless (memq sym solarized-test--emphatic-faces)
               (push sym overwrought))))))
      (expect overwrought :to-equal '()))))

(provide 'solarized-test)

;;; solarized-test.el ends here
