;;; colorlab-engine.el --- Colorlab calculation engine -*- lexical-binding: t -*-

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

;; Color space conversions, blending, lightness/chroma adjustments,
;; and accent pair generation machinery for colorlab.

;;; Code:

(require 'cl-lib)
(require 'color)

(unless (fboundp 'color-srgb-to-oklab)
  (error "Colorlab requires Emacs 30.1 or later (Oklab support in color.el)"))

;;; Color space conversions

;; The conversions between sRGB, CIE L*a*b*, CIELCh, HSL and Oklab come from
;; color.el.  Hex parsing and formatting stay here: `color-name-to-rgb' goes
;; through the terminal's color approximation in batch mode, and
;; `color-rgb-to-hex' truncates components instead of rounding them.

(defun colorlab--get (key alist)
  "Return the value of string KEY in ALIST."
  (alist-get key alist nil nil #'string=))

(defun colorlab--lerp3 (a b t-val)
  "Interpolate between the 3-vectors A and B at ratio T-VAL in [0..1]."
  (cl-mapcar (lambda (x y) (+ x (* t-val (- y x)))) a b))

(defun colorlab-hex-to-rgb (hex)
  "Convert HEX color string to RGB components in [0.0..1.0].
HEX can be \"#rrggbb\" or \"rrggbb\"."
  (let ((hex (if (string-prefix-p "#" hex) (substring hex 1) hex))
        (factor (/ 1.0 255.0)))
    (mapcar (lambda (i) (* (string-to-number (substring hex i (+ i 2)) 16) factor))
            '(0 2 4))))

(defun colorlab-rgb-to-hex (rgb)
  "Convert RGB components [0.0..1.0] to a hex color string \"#rrggbb\".
Components are clamped to [0.0..1.0] and rounded."
  (apply #'format "#%02x%02x%02x"
         (mapcar (lambda (v) (floor (+ (* (color-clamp v) 255.0) 0.5))) rgb)))

;;; CIE L*a*b* and CIELCh

(defun colorlab-hex-to-lab (hex)
  "Convert HEX color string to CIE L*a*b* coordinates (L in [0..100])."
  (apply #'color-srgb-to-lab (colorlab-hex-to-rgb hex)))

(defun colorlab-lab-to-hex (lab)
  "Convert CIE L*a*b* coordinates LAB (L in [0..100]) to HEX color string."
  (colorlab-rgb-to-hex (apply #'color-lab-to-srgb lab)))

(defun colorlab-blend-lab (c1-hex c2-hex t-val)
  "Blend two hex colors C1-HEX and C2-HEX in L*a*b* space at ratio T-VAL in [0..1]."
  (colorlab-lab-to-hex (colorlab--lerp3 (colorlab-hex-to-lab c1-hex)
                                        (colorlab-hex-to-lab c2-hex)
                                        t-val)))

(defun colorlab-hex-to-lch (hex)
  "Convert HEX color string to CIELCh coordinates (L C h).
L is in [0..100], and h is the hue angle in degrees [0.0..360.0)."
  (cl-destructuring-bind (l c h) (apply #'color-lab-to-lch (colorlab-hex-to-lab hex))
    (list l c (mod (radians-to-degrees h) 360.0))))

(defun colorlab-lch-to-hex (lch)
  "Convert CIELCh coordinates LCH (L C h, h in degrees) to HEX color string."
  (cl-destructuring-bind (l c h) lch
    (colorlab-lab-to-hex (color-lch-to-lab l c (degrees-to-radians h)))))

(defun colorlab-change-hue (hex degrees)
  "Adjust hue of HEX color by DEGREES in CIELCh space."
  (cl-destructuring-bind (l c h) (colorlab-hex-to-lch hex)
    (colorlab-lch-to-hex (list l c (mod (+ h degrees) 360.0)))))

(defun colorlab-change-chroma (hex amount)
  "Adjust chroma of HEX color by AMOUNT in CIELCh space."
  (cl-destructuring-bind (l c h) (colorlab-hex-to-lch hex)
    (colorlab-lch-to-hex (list l (max 0.0 (+ c amount)) h))))

;;; HSL

(defun colorlab-change-saturation (hex amount)
  "Adjust saturation of HEX color by AMOUNT in HSL space."
  (cl-destructuring-bind (h s l) (apply #'color-rgb-to-hsl (colorlab-hex-to-rgb hex))
    (colorlab-rgb-to-hex (color-hsl-to-rgb h (+ s amount) l))))

(defun colorlab-change-lightness (hex amount)
  "Adjust lightness of HEX color by AMOUNT in Oklab space."
  (let* ((oklab (colorlab-hex-to-oklab hex))
         (l (min 1.0 (max 0.0 (+ (nth 0 oklab) amount))))
         (a (nth 1 oklab))
         (b (nth 2 oklab)))
    (colorlab-oklab-to-hex (list l a b))))

;;; Oklab & Oklch color spaces (Björn Ottosson)

(defun colorlab-hex-to-oklab (hex)
  "Convert HEX color string to Oklab coordinates (L a b).
L is in [0.0..1.0], a (green-red) and b (blue-yellow) in [-0.4..0.4]."
  (apply #'color-srgb-to-oklab (colorlab-hex-to-rgb hex)))

(defun colorlab-oklab-to-hex (oklab)
  "Convert Oklab coordinates OKLAB (L a b) to HEX color string."
  (colorlab-rgb-to-hex (apply #'color-oklab-to-srgb oklab)))

(defun colorlab-oklab-to-oklch (oklab)
  "Convert OKLAB (L a b) coordinates to Oklch (L C h).
L is in [0.0..1.0], C >= 0.0, and h is hue angle in [0.0..360.0)."
  (let* ((l (nth 0 oklab))
         (a (nth 1 oklab))
         (b (nth 2 oklab))
         (c (sqrt (+ (* a a) (* b b))))
         (h-deg (* (atan b a) (/ 180.0 float-pi)))
         (h (if (< h-deg 0.0) (+ h-deg 360.0) h-deg)))
    (list l c h)))

(defun colorlab-oklch-to-oklab (oklch)
  "Convert OKLCH (L C h) coordinates to Oklab (L a b)."
  (let* ((l (nth 0 oklch))
         (c (nth 1 oklch))
         (h (nth 2 oklch))
         (h-rad (* h (/ float-pi 180.0)))
         (a (* c (cos h-rad)))
         (b (* c (sin h-rad))))
    (list l a b)))

(defun colorlab-hex-to-oklch (hex)
  "Convert HEX color string to Oklch coordinates (L C h)."
  (colorlab-oklab-to-oklch (colorlab-hex-to-oklab hex)))

(defun colorlab-oklch-to-hex (oklch)
  "Convert OKLCH (L C h) coordinates to HEX color string."
  (colorlab-oklab-to-hex (colorlab-oklch-to-oklab oklch)))

(defun colorlab-blend-oklab (c1-hex c2-hex t-val)
  "Blend two hex colors C1-HEX and C2-HEX in Oklab space at ratio T-VAL.
T-VAL is in [0..1]."
  (colorlab-oklab-to-hex (colorlab--lerp3 (colorlab-hex-to-oklab c1-hex)
                                          (colorlab-hex-to-oklab c2-hex)
                                          t-val)))

(defun colorlab-change-oklab-lightness (hex amount)
  "Adjust lightness of HEX color by AMOUNT in Oklab space."
  (let* ((oklab (colorlab-hex-to-oklab hex))
         (l (min 1.0 (max 0.0 (+ (nth 0 oklab) amount))))
         (a (nth 1 oklab))
         (b (nth 2 oklab)))
    (colorlab-oklab-to-hex (list l a b))))

(defun colorlab-change-oklch-hue (hex degrees)
  "Adjust hue of HEX color by DEGREES in Oklch space."
  (let* ((oklch (colorlab-hex-to-oklch hex))
         (l (nth 0 oklch))
         (c (nth 1 oklch))
         (h (mod (+ (nth 2 oklch) degrees) 360.0)))
    (colorlab-oklch-to-hex (list l c (if (< h 0.0) (+ h 360.0) h)))))

(defun colorlab-change-oklch-chroma (hex amount)
  "Adjust chroma of HEX color by AMOUNT in Oklch space."
  (let* ((oklch (colorlab-hex-to-oklch hex))
         (l (nth 0 oklch))
         (c (max 0.0 (+ (nth 1 oklch) amount)))
         (h (nth 2 oklch)))
    (colorlab-oklch-to-hex (list l c h))))

(defun colorlab-modify-oklch (hex &optional l c h)
  "Return HEX with Oklch components optionally replaced by L, C, and H."
  (let* ((oklch (colorlab-hex-to-oklch hex)))
    (colorlab-oklch-to-hex (list (or l (nth 0 oklch))
                                 (or c (nth 1 oklch))
                                 (or h (nth 2 oklch))))))

(defun colorlab-set-oklch-hue (hex hue)
  "Set the absolute HUE angle (in degrees [0..360]) of HEX in Oklch space.
Lightness and chroma are preserved."
  (colorlab-modify-oklch hex nil nil hue))

(defun colorlab-set-oklch-chroma (hex chroma)
  "Set the absolute CHROMA of HEX in Oklch space.
Lightness and hue are preserved."
  (colorlab-modify-oklch hex nil chroma nil))

(defconst colorlab-base-names
  '("base03" "base02" "base01" "base00" "base0" "base1" "base2" "base3")
  "Names of the 8 Solarized base tones in order.")

(defconst colorlab-accent-names
  '("yellow" "orange" "red" "magenta" "violet" "blue" "cyan" "green")
  "Names of the 8 Solarized accent colors in order.")

(defconst colorlab-all-names
  (append colorlab-base-names colorlab-accent-names)
  "All core names used for sorting.")

(defun colorlab-change-base-lightness (base amount-fgs amount-bgs)
  "Adjust lightness of BASE colors by AMOUNT-FGS and AMOUNT-BGS.
base03, base02 and base00 are adjusted by AMOUNT-BGS, the rest by AMOUNT-FGS."
  (mapcar (lambda (name)
            (cons name (colorlab-change-lightness
                        (colorlab--get name base)
                        (if (member name '("base03" "base02" "base00")) amount-bgs amount-fgs))))
          colorlab-base-names))

(defun colorlab-change-accents-lightness (accents amount)
  "Adjust lightness of all ACCENTS by AMOUNT in Oklab space."
  (mapcar (lambda (pair)
            (cons (car pair)
                  (colorlab-change-lightness (cdr pair) amount)))
          accents))

(defun colorlab-inverse-base (base)
  "Invert BASE colors (swap base03..base00 with base3..base0)."
  (cl-mapcar (lambda (name source) (cons name (colorlab--get source base)))
             colorlab-base-names
             (reverse colorlab-base-names)))

(defun colorlab-sort-keys (keys)
  "Sort KEYS in Solarized order: base names, accents, then suffixes."
  (let* ((n (length colorlab-all-names))
         (score (lambda (key)
                  (let ((i (cl-position-if (lambda (name) (string-match-p (regexp-quote name) key))
                                           colorlab-all-names)))
                    (cond ((null i) 0)
                          ((string= key (nth i colorlab-all-names)) (+ (- (1+ n) i) n))
                          (t (- (1+ n) i)))))))
    (cl-sort (sort (copy-sequence keys) #'string<) #'> :key score)))

;;; Accent Pair Generation

;; Default pair parameters, defined in colorlab.el.
(defvar colorlab-default-accent1)
(defvar colorlab-default-accent2)

(defconst colorlab-pair-suffixes '("-1bg" "-1fg" "-2bg" "-2fg")
  "Name suffixes of the generated accent pair colors.")

(defun colorlab-generate-accent-pair (accents base params)
  "Generate background and foreground accent pairs for ACCENTS against BASE.
PARAMS is a plist with :bg-amt, :fg-amt, :gamma, :min-diff, :fg-finder
and :bg-finder.  A finder is a base name, or \\='extreme-bg or
\\='extreme-fg for the darkest or lightest base tone.  Return a list
\(BGS FGS) of alists.  Calculations are performed in Oklab space.

The optional :bg-tint K gives each background the hue of its accent
and K times the chroma a blend toward a neutral tone would leave.
Without it, backgrounds take on the hue of the blend target, which
mutes hues opposite to a tinted page (blue on cream turns gray)."
  (cl-destructuring-bind (&key bg-amt fg-amt gamma min-diff fg-finder bg-finder bg-tint) params
    (let* ((lightness (lambda (hex) (car (colorlab-hex-to-oklab hex))))
           (dark-on-bright (> (funcall lightness (colorlab--get "base0" base))
                              (funcall lightness (colorlab--get "base03" base))))
           (by-lightness (mapcar #'cdr (sort (copy-sequence base)
                                             (lambda (a b)
                                               (< (funcall lightness (cdr a))
                                                  (funcall lightness (cdr b)))))))
           (darkest (car by-lightness))
           (lightest (car (last by-lightness)))
           (find (lambda (finder)
                   (pcase finder
                     ('extreme-bg (if dark-on-bright darkest lightest))
                     ('extreme-fg (if dark-on-bright lightest darkest))
                     ((pred stringp) (colorlab--get finder base))
                     (_ (error "Unknown finder: %S" finder)))))
           (bg-blend (colorlab-hex-to-oklab (funcall find bg-finder)))
           (fg-blend (colorlab-hex-to-oklab (funcall find fg-finder)))
           (bgs nil)
           (fgs nil))
      (dolist (name colorlab-accent-names)
        (let* ((v (colorlab-hex-to-oklab (colorlab--get name accents)))
               (bg (colorlab--lerp3 v bg-blend bg-amt))
               (fg (colorlab--lerp3 v fg-blend fg-amt))
               (bl (car bg))
               (fl (car fg))
               (nlb (if (> fl bl) (- bl gamma) (+ bl gamma)))
               (nlf (if (> fl bl) (- fl gamma) (+ fl gamma))))
          (when (< (abs (- nlf nlb)) min-diff)
            (let ((diff (- min-diff (abs (- (abs nlf) (abs nlb)))))
                  (m (if (> bl fl) -1.0 1.0))
                  ;; (FG-WEIGHT . BG-WEIGHT) of the lightness correction.
                  (w (cond ;; For highlights (Pair 2), preserve elevated background prominence
                           ((< bg-amt 0.70) '(0.75 . 0.25))
                           ;; For subtle base tints (Pair 1)
                           (t '(0.50 . 0.50)))))
              (setq nlf (+ nlf (* nlf diff m (car w)))
                    nlb (- nlb (* nlb diff m (cdr w))))))
          (push (cons name (colorlab-oklab-to-hex
                            (cons nlb (if bg-tint
                                          (mapcar (lambda (x) (* bg-tint (- 1.0 bg-amt) x)) (cdr v))
                                        (cdr bg)))))
                bgs)
          (push (cons name (colorlab-oklab-to-hex (cons nlf (cdr fg)))) fgs)))
      (list (nreverse bgs) (nreverse fgs)))))

(defun colorlab-accent-pairs (accents base pair1-params pair2-params)
  "Return the NAME-1bg, NAME-1fg, NAME-2bg and NAME-2fg colors for ACCENTS.
The pairs are generated against BASE using PAIR1-PARAMS and PAIR2-PARAMS,
see `colorlab-generate-accent-pair' for these."
  (cl-mapcan (lambda (suffix colors)
               (mapcar (lambda (p) (cons (concat (car p) suffix) (cdr p))) colors))
             colorlab-pair-suffixes
             (append (colorlab-generate-accent-pair accents base pair1-params)
                     (colorlab-generate-accent-pair accents base pair2-params))))

;;; Palette Generation

(defun colorlab--accent-variants (accents suffix target shift)
  "Return ACCENTS, renamed with SUFFIX, saturated, blended with TARGET and shifted.
The lightness is shifted by SHIFT in Oklab space."
  (mapcar (lambda (pair)
            (cons (concat (car pair) suffix)
                  (colorlab-change-lightness
                   (colorlab-blend-oklab (colorlab-change-saturation (cdr pair) 0.2) target 0.2)
                   shift)))
          accents))

(defun colorlab-generate-palette-alist (pal-def)
  "Generate complete color alist for palette definition PAL-DEF."
  (let* ((solarized (plist-get pal-def :solarized))
         (base (plist-get solarized :base))
         (accents (plist-get solarized :accents))
         (gen-base (if (plist-get pal-def :inverse) (colorlab-inverse-base base) base))
         (base03 (colorlab--get "base03" base))
         ;; Light palettes have light base03 tones (> 0.5 L); blending with them lightens
         ;; the accent, so a stronger negative shift is needed to reach readable text contrast.
         (d-shift (if (> (car (colorlab-hex-to-oklab base03)) 0.5) -0.26 -0.10))
         (all-colors (append base
                             accents
                             (colorlab-accent-pairs
                              accents gen-base
                              (or (plist-get pal-def :accent1) colorlab-default-accent1)
                              (or (plist-get pal-def :accent2) colorlab-default-accent2))
                             (colorlab--accent-variants accents "-d" base03 d-shift)
                             (colorlab--accent-variants accents "-l" (colorlab--get "base2" base) 0.05))))
    (mapcar (lambda (k) (cons k (colorlab--get k all-colors)))
            (colorlab-sort-keys (delete-dups (mapcar #'car all-colors))))))

(defun colorlab-format-alist (nc &optional indent)
  "Format NC (an alist of color name . hex) into a string with INDENT spaces."
  (let* ((indent (or indent 4))
         (prefix (make-string indent ?\s))
         (longest 0))
    (dolist (pair nc)
      (setq longest (max longest (length (car pair)))))
    (with-temp-buffer
      (dolist (pair nc)
        (let* ((key (car pair))
               (val (cdr pair))
               (pad (make-string (- longest (length key)) ?\s)))
          (insert (format "%s(%s%s . \"%s\")\n" prefix key pad val))))
      (buffer-string))))


(provide 'colorlab-engine)
;;; colorlab-engine.el ends here
