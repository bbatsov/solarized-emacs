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
;; It translates color palette generation into Elisp, using CIE L*a*b* color
;; space blending and lightness adjustments to compute the accent pairs and
;; light/dark variants.
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

(declare-function rainbow-mode "rainbow-mode" (&optional arg))

;;; Color space conversions (sRGB, CIE XYZ, CIE L*a*b*, CIELCh, HSL, Oklab, Oklch)

(defun colorlab-hex-to-rgb (hex)
  "Convert HEX color string to RGB components in [0.0..1.0].
HEX can be \"#rrggbb\" or \"rrggbb\"."
  (let* ((hex (if (string-prefix-p "#" hex) (substring hex 1) hex))
         (factor (/ 1.0 255.0)))
    (list (* (string-to-number (substring hex 0 2) 16) factor)
          (* (string-to-number (substring hex 2 4) 16) factor)
          (* (string-to-number (substring hex 4 6) 16) factor))))

(defun colorlab-clamp01 (v)
  "Clamp V to range [0.0..1.0]."
  (min 1.0 (max 0.0 v)))

(defun colorlab-rgb-to-hex (rgb)
  "Convert RGB components [0.0..1.0] to a hex color string \"#rrggbb\"."
  (format "#%02x%02x%02x"
          (floor (+ (* (colorlab-clamp01 (nth 0 rgb)) 255.0) 0.5))
          (floor (+ (* (colorlab-clamp01 (nth 1 rgb)) 255.0) 0.5))
          (floor (+ (* (colorlab-clamp01 (nth 2 rgb)) 255.0) 0.5))))

(defun colorlab-linearize (v)
  "Convert non-linear sRGB component V in [0.0..1.0] to linear RGB."
  (if (<= v 0.04045)
      (/ v 12.92)
    (expt (/ (+ v 0.055) 1.055) 2.4)))

(defun colorlab-delinearize (v)
  "Convert linear RGB component V to non-linear sRGB."
  (if (<= v 0.0031308)
      (* 12.92 v)
    (- (* 1.055 (expt v (/ 1.0 2.4))) 0.055)))

(defun colorlab-rgb-to-xyz (rgb)
  "Convert sRGB components RGB to CIE XYZ using Bruce Lindbloom D65 matrix."
  (let ((r (colorlab-linearize (nth 0 rgb)))
        (g (colorlab-linearize (nth 1 rgb)))
        (b (colorlab-linearize (nth 2 rgb))))
    (list (+ (* 0.41239079926595948 r) (* 0.35758433938387796 g) (* 0.18048078840183429 b))
          (+ (* 0.21263900587151036 r) (* 0.71516867876775593 g) (* 0.072192315360733715 b))
          (+ (* 0.019330818715591851 r) (* 0.11919477979462599 g) (* 0.95053215224966058 b)))))

(defun colorlab-xyz-to-rgb (xyz)
  "Convert CIE XYZ coordinates to sRGB components."
  (let* ((x (nth 0 xyz))
         (y (nth 1 xyz))
         (z (nth 2 xyz))
         (r (+ (* 3.2409699419045214 x) (* -1.5373831775700935 y) (* -0.49861076029300328 z)))
         (g (+ (* -0.96924363628087983 x) (* 1.8759675015077207 y) (* 0.041555057407175613 z)))
         (b (+ (* 0.055630079696993609 x) (* -0.20397695888897657 y) (* 1.0569715142428786 z))))
    (list (colorlab-delinearize r)
          (colorlab-delinearize g)
          (colorlab-delinearize b))))

(defconst colorlab-d65 '(0.95047 1.00000 1.08883)
  "D65 standard illuminant reference white point.")

(defun colorlab-lab-f (t-val)
  "Transfer function for XYZ to Lab."
  (if (> t-val (* (/ 6.0 29.0) (/ 6.0 29.0) (/ 6.0 29.0)))
      (expt t-val (/ 1.0 3.0))
    (+ (* (/ t-val 3.0) (/ 29.0 6.0) (/ 29.0 6.0)) (/ 4.0 29.0))))

(defun colorlab-xyz-to-lab (xyz)
  "Convert CIE XYZ coordinates to CIE L*a*b*.
L is in [0.0..1.0], and a, b are in [-1.0..1.0]."
  (let* ((x (nth 0 xyz))
         (y (nth 1 xyz))
         (z (nth 2 xyz))
         (fy (colorlab-lab-f (/ y (nth 1 colorlab-d65))))
         (l (- (* 1.16 fy) 0.16))
         (a (* 5.0 (- (colorlab-lab-f (/ x (nth 0 colorlab-d65))) fy)))
         (b (* 2.0 (- fy (colorlab-lab-f (/ z (nth 2 colorlab-d65)))))))
    (list l a b)))

(defun colorlab-lab-finv (t-val)
  "Inverse transfer function for Lab to XYZ."
  (if (> t-val (/ 6.0 29.0))
      (* t-val t-val t-val)
    (* 3.0 (/ 6.0 29.0) (/ 6.0 29.0) (- t-val (/ 4.0 29.0)))))

(defun colorlab-lab-to-xyz (lab)
  "Convert CIE L*a*b* coordinates to CIE XYZ."
  (let* ((l (nth 0 lab))
         (a (nth 1 lab))
         (b (nth 2 lab))
         (l2 (/ (+ l 0.16) 1.16)))
    (list (* (nth 0 colorlab-d65) (colorlab-lab-finv (+ l2 (/ a 5.0))))
          (* (nth 1 colorlab-d65) (colorlab-lab-finv l2))
          (* (nth 2 colorlab-d65) (colorlab-lab-finv (- l2 (/ b 2.0)))))))

(defun colorlab-hex-to-lab (hex)
  "Convert HEX color string to CIE L*a*b* coordinates."
  (colorlab-xyz-to-lab (colorlab-rgb-to-xyz (colorlab-hex-to-rgb hex))))

(defun colorlab-lab-to-hex (lab)
  "Convert CIE L*a*b* coordinates to HEX color string."
  (colorlab-rgb-to-hex (colorlab-xyz-to-rgb (colorlab-lab-to-xyz lab))))

(defun colorlab-blend-lab (c1-hex c2-hex t-val)
  "Blend two hex colors C1-HEX and C2-HEX in L*a*b* space at ratio T-VAL in [0..1]."
  (let ((l1 (colorlab-hex-to-lab c1-hex))
        (l2 (colorlab-hex-to-lab c2-hex)))
    (colorlab-lab-to-hex
     (list (+ (nth 0 l1) (* t-val (- (nth 0 l2) (nth 0 l1))))
           (+ (nth 1 l1) (* t-val (- (nth 1 l2) (nth 1 l1))))
           (+ (nth 2 l1) (* t-val (- (nth 2 l2) (nth 2 l1))))))))

;;; CIELCh (Cylindrical CIE L*a*b*)

(defun colorlab-lab-to-lch (lab)
  "Convert CIE L*a*b* coordinates LAB to CIELCh (L C h).
L is in [0.0..1.0], C >= 0.0, and h is hue angle in [0.0..360.0)."
  (let* ((l (nth 0 lab))
         (a (nth 1 lab))
         (b (nth 2 lab))
         (c (sqrt (+ (* a a) (* b b))))
         (h-deg (* (atan b a) (/ 180.0 float-pi)))
         (h (if (< h-deg 0.0) (+ h-deg 360.0) h-deg)))
    (list l c h)))

(defun colorlab-lch-to-lab (lch)
  "Convert CIELCh coordinates LCH (L C h) to CIE L*a*b*."
  (let* ((l (nth 0 lch))
         (c (nth 1 lch))
         (h (nth 2 lch))
         (h-rad (* h (/ float-pi 180.0)))
         (a (* c (cos h-rad)))
         (b (* c (sin h-rad))))
    (list l a b)))

(defun colorlab-hex-to-lch (hex)
  "Convert HEX color string to CIELCh coordinates (L C h)."
  (colorlab-lab-to-lch (colorlab-hex-to-lab hex)))

(defun colorlab-lch-to-hex (lch)
  "Convert CIELCh coordinates LCH (L C h) to HEX color string."
  (colorlab-lab-to-hex (colorlab-lch-to-lab lch)))

(defun colorlab-change-hue (hex degrees)
  "Adjust hue of HEX color by DEGREES in CIELCh space."
  (let* ((lch (colorlab-hex-to-lch hex))
         (l (nth 0 lch))
         (c (nth 1 lch))
         (h (mod (+ (nth 2 lch) degrees) 360.0)))
    (colorlab-lch-to-hex (list l c (if (< h 0.0) (+ h 360.0) h)))))

(defun colorlab-change-chroma (hex amount)
  "Adjust chroma of HEX color by AMOUNT in CIELCh space."
  (let* ((lch (colorlab-hex-to-lch hex))
         (l (nth 0 lch))
         (c (max 0.0 (+ (nth 1 lch) amount)))
         (h (nth 2 lch)))
    (colorlab-lch-to-hex (list l c h))))

(defun colorlab-rgb-to-hsl (rgb)
  "Convert RGB components to HSL: H in [0..360], S in [0..1], L in [0..1]."
  (let* ((r (nth 0 rgb))
         (g (nth 1 rgb))
         (b (nth 2 rgb))
         (min-val (min r g b))
         (max-val (max r g b))
         (l (/ (+ max-val min-val) 2.0)))
    (if (= min-val max-val)
        (list 0.0 0.0 l)
      (let* ((s (if (< l 0.5)
                    (/ (- max-val min-val) (+ max-val min-val))
                  (/ (- max-val min-val) (- 2.0 max-val min-val))))
             (h (cond
                 ((= max-val r) (/ (- g b) (- max-val min-val)))
                 ((= max-val g) (+ 2.0 (/ (- b r) (- max-val min-val))))
                 (t (+ 4.0 (/ (- r g) (- max-val min-val))))))
             (h-deg (* h 60.0))
             (h-norm (if (< h-deg 0.0) (+ h-deg 360.0) h-deg)))
        (list h-norm s l)))))

(defun colorlab-hsl-to-rgb (hsl)
  "Convert HSL components to RGB."
  (let ((h (nth 0 hsl))
        (s (nth 1 hsl))
        (l (nth 2 hsl)))
    (if (= s 0.0)
        (list l l l)
      (let* ((t1 (if (< l 0.5)
                     (* l (+ 1.0 s))
                   (- (+ l s) (* l s))))
             (t2 (- (* 2.0 l) t1))
             (h-norm (/ h 360.0))
             (calc-c (lambda (tc)
                       (let ((tc (cond ((< tc 0.0) (+ tc 1.0))
                                       ((> tc 1.0) (- tc 1.0))
                                       (t tc))))
                         (cond
                          ((< (* 6.0 tc) 1.0) (+ t2 (* (- t1 t2) 6.0 tc)))
                          ((< (* 2.0 tc) 1.0) t1)
                          ((< (* 3.0 tc) 2.0) (+ t2 (* (- t1 t2) (- (/ 2.0 3.0) tc) 6.0)))
                          (t t2))))))
        (list (funcall calc-c (+ h-norm (/ 1.0 3.0)))
              (funcall calc-c h-norm)
              (funcall calc-c (- h-norm (/ 1.0 3.0))))))))

(defun colorlab-change-saturation (hex amount)
  "Adjust saturation of HEX color by AMOUNT in HSL space."
  (let* ((rgb (colorlab-hex-to-rgb hex))
         (hsl (colorlab-rgb-to-hsl rgb))
         (h (nth 0 hsl))
         (s (+ (nth 1 hsl) amount))
         (l (nth 2 hsl)))
    (colorlab-rgb-to-hex (colorlab-hsl-to-rgb (list h s l)))))

(defun colorlab-change-lightness (hex amount)
  "Adjust lightness of HEX color by AMOUNT in L*a*b* space."
  (let* ((lab (colorlab-hex-to-lab hex))
         (l (+ (nth 0 lab) amount))
         (a (nth 1 lab))
         (b (nth 2 lab)))
    (colorlab-lab-to-hex (list l a b))))

;;; Oklab & Oklch color spaces (Björn Ottosson)

(defun colorlab-rgb-to-oklab (rgb)
  "Convert sRGB components RGB to Oklab coordinates (L a b).
L is in [0.0..1.0], a (green-red) and b (blue-yellow) in [-0.4..0.4]."
  (let* ((r (colorlab-linearize (nth 0 rgb)))
         (g (colorlab-linearize (nth 1 rgb)))
         (b (colorlab-linearize (nth 2 rgb)))
         (l (+ (* 0.4122214708 r) (* 0.5363325363 g) (* 0.0514459929 b)))
         (m (+ (* 0.2119034982 r) (* 0.6806995451 g) (* 0.1073969566 b)))
         (s (+ (* 0.0883024619 r) (* 0.2817188376 g) (* 0.6299787005 b)))
         (l_ (if (> l 0.0) (expt l (/ 1.0 3.0)) (- (expt (- l) (/ 1.0 3.0)))))
         (m_ (if (> m 0.0) (expt m (/ 1.0 3.0)) (- (expt (- m) (/ 1.0 3.0)))))
         (s_ (if (> s 0.0) (expt s (/ 1.0 3.0)) (- (expt (- s) (/ 1.0 3.0))))))
    (list (+ (* 0.2104542553 l_) (* 0.7936177850 m_) (* -0.0040720468 s_))
          (+ (* 1.9779984951 l_) (* -2.4285922050 m_) (* 0.4505937099 s_))
          (+ (* 0.0259040371 l_) (* 0.7827717662 m_) (* -0.8086757660 s_)))))

(defun colorlab-oklab-to-rgb (oklab)
  "Convert Oklab coordinates OKLAB (L a b) to sRGB components."
  (let* ((L (nth 0 oklab))
         (a (nth 1 oklab))
         (b (nth 2 oklab))
         (l_ (+ L (* 0.3963377774 a) (* 0.2158037573 b)))
         (m_ (- L (* 0.1055613458 a) (* 0.0638541728 b)))
         (s_ (- L (* 0.0894841775 a) (* 1.2914855480 b)))
         (l (* l_ l_ l_))
         (m (* m_ m_ m_))
         (s (* s_ s_ s_))
         (r (+ (* 4.0767245293 l) (* -3.3072168827 m) (* 0.2307590544 s)))
         (g (+ (* -1.2681437731 l) (* 2.6093323187 m) (* -0.3411344233 s)))
         (b (+ (* -0.0041119885 l) (* -0.7034763148 m) (* 1.7068625689 s))))
    (list (colorlab-delinearize r)
          (colorlab-delinearize g)
          (colorlab-delinearize b))))

(defun colorlab-hex-to-oklab (hex)
  "Convert HEX color string to Oklab coordinates (L a b)."
  (colorlab-rgb-to-oklab (colorlab-hex-to-rgb hex)))

(defun colorlab-oklab-to-hex (oklab)
  "Convert Oklab coordinates OKLAB (L a b) to HEX color string."
  (colorlab-rgb-to-hex (colorlab-oklab-to-rgb oklab)))

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
  (let ((o1 (colorlab-hex-to-oklab c1-hex))
        (o2 (colorlab-hex-to-oklab c2-hex)))
    (colorlab-oklab-to-hex
     (list (+ (nth 0 o1) (* t-val (- (nth 0 o2) (nth 0 o1))))
           (+ (nth 1 o1) (* t-val (- (nth 1 o2) (nth 1 o1))))
           (+ (nth 2 o1) (* t-val (- (nth 2 o2) (nth 2 o1))))))))

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

(defun colorlab-change-base-lightness (base amount-fgs amount-bgs)
  "Adjust lightness of BASE colors by AMOUNT-FGS and AMOUNT-BGS."
  (let ((b03 (colorlab-change-lightness (alist-get "base03" base nil nil #'string=) amount-bgs))
        (b02 (colorlab-change-lightness (alist-get "base02" base nil nil #'string=) amount-bgs))
        (b01 (colorlab-change-lightness (alist-get "base01" base nil nil #'string=) amount-fgs))
        (b00 (colorlab-change-lightness (alist-get "base00" base nil nil #'string=) amount-bgs))
        (b0  (colorlab-change-lightness (alist-get "base0"  base nil nil #'string=) amount-fgs))
        (b1  (colorlab-change-lightness (alist-get "base1"  base nil nil #'string=) amount-fgs))
        (b2  (colorlab-change-lightness (alist-get "base2"  base nil nil #'string=) amount-fgs))
        (b3  (colorlab-change-lightness (alist-get "base3"  base nil nil #'string=) amount-fgs)))
    `(("base03" . ,b03)
      ("base02" . ,b02)
      ("base01" . ,b01)
      ("base00" . ,b00)
      ("base0"  . ,b0)
      ("base1"  . ,b1)
      ("base2"  . ,b2)
      ("base3"  . ,b3))))

(defun colorlab-change-accents-lightness (accents amount)
  "Adjust lightness of all ACCENTS by AMOUNT in L*a*b* space."
  (mapcar (lambda (pair)
            (cons (car pair)
                  (colorlab-change-lightness (cdr pair) amount)))
          accents))

(defun colorlab-inverse-base (base)
  "Invert BASE colors (swap base03..base00 with base3..base0)."
  `(("base03" . ,(alist-get "base3"  base nil nil #'string=))
    ("base02" . ,(alist-get "base2"  base nil nil #'string=))
    ("base01" . ,(alist-get "base1"  base nil nil #'string=))
    ("base00" . ,(alist-get "base0"  base nil nil #'string=))
    ("base0"  . ,(alist-get "base00" base nil nil #'string=))
    ("base1"  . ,(alist-get "base01" base nil nil #'string=))
    ("base2"  . ,(alist-get "base02" base nil nil #'string=))
    ("base3"  . ,(alist-get "base03" base nil nil #'string=))))

(defconst colorlab-base-names
  '("base03" "base02" "base01" "base00" "base0" "base1" "base2" "base3")
  "Names of the 8 Solarized base tones in order.")

(defconst colorlab-accent-names
  '("yellow" "orange" "red" "magenta" "violet" "blue" "cyan" "green")
  "Names of the 8 Solarized accent colors in order.")

(defconst colorlab-all-names
  (append colorlab-base-names colorlab-accent-names)
  "All core names used for sorting.")

(defun colorlab-sort-keys (keys)
  "Sort KEYS in Solarized order: base names, accents, then suffixes."
  (let* ((sorted (sort (copy-sequence keys) #'string<))
         (all-len (length colorlab-all-names))
         (score (lambda (key)
                  (let ((found-idx nil)
                        (exact nil)
                        (i 0))
                    (while (and (< i all-len) (null found-idx))
                      (let ((name (nth i colorlab-all-names)))
                        (if (string-match-p (regexp-quote name) key)
                            (progn
                              (setq found-idx i)
                              (setq exact (string= key name)))
                          (setq i (1+ i)))))
                    (if found-idx
                        (+ (- (1+ all-len) found-idx) (if exact all-len 0))
                      0)))))
    (sort sorted (lambda (a b)
                   (> (funcall score a) (funcall score b))))))

;;; Accent Pair Generation

(defun colorlab-generate-accent-pair (accents base bg-amt fg-amt gamma min-diff fg-finder bg-finder)
  "Generate background and foreground accent pairs for ACCENTS against BASE."
  (let ((dark-on-bright (> (car (colorlab-hex-to-lab (alist-get "base0" base nil nil #'string=)))
                           (car (colorlab-hex-to-lab (alist-get "base03" base nil nil #'string=)))))
        (sorted-base-by-lightness
         (sort (copy-sequence base)
               (lambda (a b)
                 (< (car (colorlab-hex-to-lab (cdr a)))
                    (car (colorlab-hex-to-lab (cdr b)))))))
        (bgs nil)
        (fgs nil))
    (let ((blend-bg-hex
           (cond
            ((eq bg-finder 'extreme-bg)
             (cdr (if dark-on-bright
                      (car sorted-base-by-lightness)
                    (car (last sorted-base-by-lightness)))))
            ((stringp bg-finder)
             (alist-get bg-finder base nil nil #'string=))
            (t (error "Unknown bg-finder: %S" bg-finder))))
          (blend-fg-hex
           (cond
            ((eq fg-finder 'extreme-fg)
             (cdr (if dark-on-bright
                      (car (last sorted-base-by-lightness))
                    (car sorted-base-by-lightness))))
            ((stringp fg-finder)
             (alist-get fg-finder base nil nil #'string=))
            (t (error "Unknown fg-finder: %S" fg-finder)))))
      (dolist (accent-name colorlab-accent-names)
        (let* ((v-hex (alist-get accent-name accents nil nil #'string=))
               (v-lab (colorlab-hex-to-lab v-hex))
               (bg-lab-blend (colorlab-hex-to-lab blend-bg-hex))
               (fg-lab-blend (colorlab-hex-to-lab blend-fg-hex))
               (bg-lab (list (+ (nth 0 v-lab) (* bg-amt (- (nth 0 bg-lab-blend) (nth 0 v-lab))))
                             (+ (nth 1 v-lab) (* bg-amt (- (nth 1 bg-lab-blend) (nth 1 v-lab))))
                             (+ (nth 2 v-lab) (* bg-amt (- (nth 2 bg-lab-blend) (nth 2 v-lab))))))
               (fg-lab (list (+ (nth 0 v-lab) (* fg-amt (- (nth 0 fg-lab-blend) (nth 0 v-lab))))
                             (+ (nth 1 v-lab) (* fg-amt (- (nth 1 fg-lab-blend) (nth 1 v-lab))))
                             (+ (nth 2 v-lab) (* fg-amt (- (nth 2 fg-lab-blend) (nth 2 v-lab))))))
               (bl (nth 0 bg-lab))
               (ba (nth 1 bg-lab))
               (bb (nth 2 bg-lab))
               (fl (nth 0 fg-lab))
               (fa (nth 1 fg-lab))
               (fb (nth 2 fg-lab))
               (nlb (if (> fl bl) (- bl gamma) (+ bl gamma)))
               (nlf (if (> fl bl) (- fl gamma) (+ fl gamma))))
          (when (< (- (max nlf nlb) (min nlf nlb)) min-diff)
            (let* ((diff (- min-diff (- (max (abs nlf) (abs nlb)) (min (abs nlf) (abs nlb)))))
                   (m (if (> bl fl) -1.0 1.0)))
              (setq nlf (+ nlf (* nlf diff m)))
              (setq nlb (- nlb (* nlb diff m)))))
          (push (cons accent-name (colorlab-lab-to-hex (list nlb ba bb))) bgs)
          (push (cons accent-name (colorlab-lab-to-hex (list nlf fa fb))) fgs)))
      (list (nreverse bgs) (nreverse fgs)))))

;;; Palette Generation

(defun colorlab-generate-palette-alist (pal-def)
  "Generate complete color alist for palette definition PAL-DEF."
  (let* ((solarized (plist-get pal-def :solarized))
         (base (plist-get solarized :base))
         (accents (plist-get solarized :accents))
         (inverse (plist-get pal-def :inverse))
         (gen-base (if inverse (colorlab-inverse-base base) base))
         (gen-accents (copy-sequence accents))
         (pair1-params (or (plist-get pal-def :accent1)
                           '(:bg-amt 0.85
                             :fg-amt 0.30
                             :gamma 0.01
                             :min-diff 0.35
                             :bg-finder extreme-bg
                             :fg-finder extreme-fg)))
         (pair2-params (or (plist-get pal-def :accent2)
                           '(:bg-amt 0.60
                             :fg-amt 0.45
                             :gamma 0.04
                             :min-diff 0.35
                             :bg-finder extreme-bg
                             :fg-finder extreme-fg)))
         (pair1 (colorlab-generate-accent-pair gen-accents gen-base
                                              (plist-get pair1-params :bg-amt)
                                              (plist-get pair1-params :fg-amt)
                                              (plist-get pair1-params :gamma)
                                              (plist-get pair1-params :min-diff)
                                              (plist-get pair1-params :fg-finder)
                                              (plist-get pair1-params :bg-finder)))
         (pair2 (colorlab-generate-accent-pair gen-accents gen-base
                                              (plist-get pair2-params :bg-amt)
                                              (plist-get pair2-params :fg-amt)
                                              (plist-get pair2-params :gamma)
                                              (plist-get pair2-params :min-diff)
                                              (plist-get pair2-params :fg-finder)
                                              (plist-get pair2-params :bg-finder)))
         (bgs (nth 0 pair1))
         (fgs (nth 1 pair1))
         (hbgs (nth 0 pair2))
         (hfgs (nth 1 pair2))
         (base03 (alist-get "base03" base nil nil #'string=))
         (base2 (alist-get "base2" base nil nil #'string=))
         (d-colors
          (mapcar (lambda (pair)
                    (cons (concat (car pair) "-d")
                          (colorlab-change-lightness
                           (colorlab-blend-lab (colorlab-change-saturation (cdr pair) 0.2) base03 0.2)
                           -0.1)))
                  accents))
         (l-colors
          (mapcar (lambda (pair)
                    (cons (concat (car pair) "-l")
                          (colorlab-change-lightness
                           (colorlab-blend-lab (colorlab-change-saturation (cdr pair) 0.2) base2 0.2)
                           0.05)))
                  accents))
         (all-colors (append base
                             accents
                             (mapcar (lambda (p) (cons (concat (car p) "-1bg") (cdr p))) bgs)
                             (mapcar (lambda (p) (cons (concat (car p) "-1fg") (cdr p))) fgs)
                             (mapcar (lambda (p) (cons (concat (car p) "-2bg") (cdr p))) hbgs)
                             (mapcar (lambda (p) (cons (concat (car p) "-2fg") (cdr p))) hfgs)
                             d-colors
                             l-colors))
         (all-keys (colorlab-sort-keys (mapcar #'car all-colors))))
    (mapcar (lambda (k)
              (cons k (alist-get k all-colors nil nil #'string=)))
            all-keys)))

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

;;; Palette Configurations

;; The original solarized color palette
(defconst colorlab-solarized
  (list
   :base
   `(("base03" . "#002b36")
     ("base02" . "#073642")
     ("base01" . "#586e75")
     ("base00" . "#657b83")
     ("base0" . "#839496")
     ("base1" . "#93a1a1")
     ("base2" . "#eee8d5")
     ("base3" . "#fdf6e3"))
   :accents
   `(("yellow" . "#b58900")
     ("orange" . "#cb4b16")
     ("red" . "#dc322f")
     ("magenta" . "#d33682")
     ("violet" . "#6c71c4")
     ("blue" . "#268bd2")
     ("cyan" . "#2aa198")
     ("green" . "#859900"))))

(defconst colorlab-solarized-dark-high-contrast
  (list :base (colorlab-change-base-lightness (plist-get colorlab-solarized :base) 0.04 -0.02)
        :accents (colorlab-change-accents-lightness (plist-get colorlab-solarized :accents) 0.05)))

(defconst colorlab-solarized-light-high-contrast
  (list :base (colorlab-change-base-lightness (plist-get colorlab-solarized :base) 0.02 -0.05)
        :accents (colorlab-change-accents-lightness (plist-get colorlab-solarized :accents) -0.05)))

(defconst colorlab-gruvbox-dark
  (list
   :base
   `(("base03" . "#282828")
     ("base02" . "#32302f")
     ("base01" . "#7c6f64")
     ("base00" . "#282828")
     ("base0" . "#a89984")
     ("base1" . "#bdae93")
     ("base2" . "#a89984")
     ("base3" . "#fbf1c7"))
   :accents
   `(("yellow" . "#d79921")
     ("orange" . "#d65d0e")
     ("red" . "#fb4933")
     ("magenta" . "#d3869b")
     ("violet" . "#b16286")
     ("blue" . "#458588")
     ("cyan" . "#689d6a")
     ("green" . "#98971a"))))

;; note: not inversed here
(defconst colorlab-gruvbox-light
  (list
   :base
   `(("base03" . "#282828")
     ("base02" . "#32302f")
     ("base01" . "#665c54")
     ("base00" . "#7c6f64")
     ("base0" . "#3c3836")
     ("base1" . "#a89984")
     ("base2" . "#ebdbb2")
     ("base3" . "#fbf1c7"))
   :accents
   `(("yellow" . "#b57614")
     ("orange" . "#af3a03")
     ("red" . "#9d0006")
     ("magenta" . "#d3869b")
     ("violet" . "#8f3f71")
     ("blue" . "#076678")
     ("cyan" . "#689d6a")
     ("green" . "#98971a"))))

(defconst colorlab-zenburn
  (list
   :base
   `(("base03" . "#3F3F3F")
     ("base02" . "#4F4F4F")
     ("base01" . ,(colorlab-blend-lab "#656555" "#DCDCCC" 0.3))
     ("base00" . "#6F6F6F")
     ("base0" . "#DCDCCC")
     ("base1" . "#FFFFEF")
     ("base2" . ,(colorlab-blend-lab "#FFFFEF" "#FFFFFD" 0.5))
     ("base3" . "#FFFFFD"))
   :accents
   `(("yellow"  . "#F0DFAF")
     ("orange"  . "#DFAF8F")
     ("red"     . "#CC9393")
     ("magenta" . "#DC8CC3")
     ("violet"  . ,(colorlab-blend-lab "#8CD0D3" "#DC8CC3" 0.5))  ;; blend of blue and magenta
     ("blue"    . "#8CD0D3")
     ("cyan"    . "#93E0E3")
     ("green"   . "#7F9F7F"))))

(defconst colorlab-monokai
  (list
   :base
   `(("base03" . "#272822")
     ("base02" . "#3E3D31")
     ("base01" . "#75715E")
     ("base00" . "#49483E")
     ("base0" . "#F8F8F2")
     ("base1" . "#F8F8F2")
     ("base2" . "#F8F8F2")
     ("base3" . "#F8F8F2"))
   :accents
   `(("yellow" . "#E6DB74")
     ("orange" . "#FD971F")
     ("red" . "#F92672")
     ("magenta" . "#FD5FF0")
     ("violet" . "#AE81FF")
     ("blue" . "#66D9EF")
     ("cyan" . "#A1EFE4")
     ("green" . "#A6E22E"))))

(defconst colorlab-selenized-dark
  (list
   :base
   `(("base03" . "#103c48")
     ("base02" . "#184956")
     ("base01" . "#72898f")
     ("base00" . "#103c48")
     ("base0" . "#adbcbc")
     ("base1" . "#cad8d9")
     ("base2" . "#ece3cc")
     ("base3" . "#fbf3db"))
   :accents
   `(("yellow" . "#dbb32d")
     ("orange" . "#ed8649")
     ("red" . "#fa5750")
     ("magenta" . "#f275be")
     ("violet" . "#af88eb")
     ("blue" . "#4695f7")
     ("cyan" . "#41c7b9")
     ("green" . "#75b938"))))

(defconst colorlab-selenized-black
  (list
   :base
   `(("base03" . "#181818")
     ("base02" . "#252525")
     ("base01" . "#777777")
     ("base00" . "#181818")
     ("base0" . "#b9b9b9")
     ("base1" . "#dedede")
     ("base2" . "#53676d")
     ("base3" . "#3a4d53"))
   :accents
   `(("yellow" . "#dbb32d")
     ("orange" . "#e67f43")
     ("red" . "#ed4a46")
     ("magenta" . "#eb6eb7")
     ("violet" . "#a580e2")
     ("blue" . "#368aeb")
     ("cyan" . "#3fc5b7")
     ("green" . "#70b433"))))

(defconst colorlab-selenized-light
  (list
   :base
   `(("base03" . "#cad8d9")
     ("base02" . "#adbcbc")
     ("base01" . "#3a4d53")
     ("base00" . "#53676d")
     ("base0" . "#fbf3db")
     ("base1" . "#909995")
     ("base2" . "#ece3cc")
     ("base3" . "#fbf3db"))
   :accents
   `(("yellow" . "#ad8900")
     ("orange" . "#c25d1e")
     ("red" . "#d2212d")
     ("magenta" . "#ca4898")
     ("violet" . "#8762c6")
     ("blue" . "#0072d4")
     ("cyan" . "#009c8f")
     ("green" . "#489100"))))

(defconst colorlab-selenized-white
  (list
   :base
   `(("base03" . "#dedede")
     ("base02" . "#b9b9b9")
     ("base01" . "#282828")
     ("base00" . "#474747")
     ("base0" . "#ffffff")
     ("base1" . "#878787")
     ("base2" . "#ebebeb")
     ("base3" . "#ffffff"))
   :accents
   `(("yellow" . "#c49700")
     ("orange" . "#d04a00")
     ("red" . "#d6000c")
     ("magenta" . "#dd0f9d")
     ("violet" . "#7f51d6")
     ("blue" . "#0064e4")
     ("cyan" . "#00ad9c")
     ("green" . "#1d9700"))))

;; Everforest dark, medium contrast (sainnhe/everforest). Base00, which some faces use as
;; text, is the comment color rather than the background.
(defconst colorlab-everforest-dark
  (list
   :base
   `(("base03" . "#2d353b")
     ("base02" . "#343f44")
     ("base01" . "#859289")
     ("base00" . "#859289")
     ("base0"  . "#d3c6aa")
     ("base1"  . "#d3c6aa")
     ("base2"  . "#f4f0d9")
     ("base3"  . "#fdf6e3"))
   :accents
   `(("yellow"  . "#dbbc7f")
     ("orange"  . "#e69875")
     ("red"     . "#e67e80")
     ("magenta" . "#d699b6")
     ("violet"  . ,(colorlab-set-oklch-hue "#d699b6" 300.0))  ;; magenta turned toward blue
     ("blue"    . "#7fbbb3")
     ("cyan"    . "#83c092")
     ("green"   . "#a7c080"))))

;; Everforest light, medium contrast (sainnhe/everforest). Base0, which some faces use as
;; text, is the comment color rather than the background.
(defconst colorlab-everforest-light
  (list
   :base
   `(("base03" . "#d3c6aa")
     ("base02" . "#d3c6aa")
     ("base01" . "#5c6a72")
     ("base00" . "#5c6a72")
     ("base0"  . "#939f91")
     ("base1"  . "#939f91")
     ("base2"  . "#f4f0d9")
     ("base3"  . "#fdf6e3"))
   :accents
   `(("yellow"  . "#dfa000")
     ("orange"  . "#f57d26")
     ("red"     . "#f85552")
     ("magenta" . "#df69ba")
     ("violet"  . ,(colorlab-modify-oklch "#df69ba" 0.65 0.14 300.0))  ;; magenta turned toward blue
     ("blue"    . "#3a94c5")
     ("cyan"    . "#35a77c")
     ("green"   . "#8da101"))))

;; Flexoki dark (stephango.com/flexoki): bg, bg-2, tx-3, tx and the 400 accents. Base00, which some faces use as
;; text, is the comment color rather than the background.
(defconst colorlab-flexoki-dark
  (list
   :base
   `(("base03" . "#100f0f")
     ("base02" . "#1c1b1a")
     ("base01" . "#575653")
     ("base00" . "#575653")
     ("base0" . "#cecdc3")
     ("base1" . "#cecdc3")
     ("base2" . "#f2f0e5")
     ("base3" . "#fffcf0"))
   :accents
   `(("yellow" . "#d0a215")
     ("orange" . "#da702c")
     ("red" . "#d14d41")
     ("magenta" . "#ce5d97")
     ("violet" . "#8b7ec8")
     ("blue" . "#4385be")
     ("cyan" . "#3aa99f")
     ("green" . "#879a39"))))

;; Flexoki light (stephango.com/flexoki): paper, bg-2, tx-3, tx and the 600 accents. Base0, which some faces use as
;; text, is the comment color rather than the background.
(defconst colorlab-flexoki-light
  (list
   :base
   `(("base03" . "#cecdc3")
     ("base02" . "#cecdc3")
     ("base01" . "#100f0f")
     ("base00" . "#100f0f")
     ("base0" . "#b7b5ac")
     ("base1" . "#b7b5ac")
     ("base2" . "#f2f0e5")
     ("base3" . "#fffcf0"))
   :accents
   `(("yellow" . "#ad8301")
     ("orange" . "#bc5215")
     ("red" . "#af3029")
     ("magenta" . "#a02f6f")
     ("violet" . "#5e409d")
     ("blue" . "#205ea6")
     ("cyan" . "#24837b")
     ("green" . "#66800b"))))

;; Kanagawa wave (rebelot/kanagawa.nvim). Base00, which some faces use as
;; text, is the comment color rather than the background.
(defconst colorlab-kanagawa-wave
  (list
   :base
   `(("base03" . "#1f1f28")
     ("base02" . "#2a2a37")
     ("base01" . "#727169")
     ("base00" . "#727169")
     ("base0" . "#dcd7ba")
     ("base1" . "#dcd7ba")
     ("base2" . "#e7dba0")
     ("base3" . "#f2ecbc"))
   :accents
   `(("yellow" . "#c0a36e")
     ("orange" . "#ffa066")
     ("red" . "#c34043")
     ("magenta" . "#d27e99")
     ("violet" . "#957fb8")
     ("blue" . "#7e9cd8")
     ("cyan" . "#6a9589")
     ("green" . "#76946a"))))

;; Kanagawa dragon (rebelot/kanagawa.nvim). Base00, which some faces use as
;; text, is the comment color rather than the background.
(defconst colorlab-kanagawa-dragon
  (list
   :base
   `(("base03" . "#181616")
     ("base02" . "#282727")
     ("base01" . "#737c73")
     ("base00" . "#737c73")
     ("base0"  . "#c5c9c5")
     ("base1"  . "#c5c9c5")
     ("base2"  . "#e7dba0")
     ("base3"  . "#f2ecbc"))
   :accents
   `(("yellow"  . "#c4b28a")
     ("orange"  . ,(colorlab-set-oklch-chroma "#b6927b" 0.09))  ;; dragonOrange with OKLCH chroma raised
     ("red"     . "#c4746e")
     ("magenta" . "#a292a3")
     ("violet"  . "#8992a7")
     ("blue"    . "#658594")  ;; dragonBlue (terminal blue and cyan are nearly identical)
     ("cyan"    . "#7aa89f")  ;; waveAqua2
     ("green"   . "#8a9a7b"))))

;; Kanagawa lotus (rebelot/kanagawa.nvim). Base0, which some faces use as
;; text, is the comment color rather than the background.
(defconst colorlab-kanagawa-lotus
  (list
   :base
   `(("base03" . "#dcd7ba")
     ("base02" . "#dcd7ba")
     ("base01" . "#43436c")
     ("base00" . "#545464")
     ("base0"  . "#8a8980")
     ("base1"  . "#8a8980")
     ("base2"  . "#e7dba0")
     ("base3"  . "#f2ecbc"))
   :accents
   `(("yellow"  . "#77713f")
     ("orange"  . "#cc6d00")
     ("red"     . "#c84053")
     ("magenta" . "#b35b79")
     ("violet"  . "#5d57a3")  ;; lotusBlue5 (lotusViolet4 is bright magenta)
     ("blue"    . "#4d699b")
     ("cyan"    . "#597b75")
     ("green"   . "#6f894e"))))

;; Nightfox. Base2 and Base3 are dayfox's backgrounds.
(defconst colorlab-nightfox
  (list
   :base
   `(("base03" . "#192330")
     ("base02" . "#212e3f")
     ("base01" . "#738091")
     ("base00" . "#738091")
     ("base0" . "#cdcecf")
     ("base1" . "#d6d6d7")
     ("base2" . "#e4dcd4")
     ("base3" . "#f6f2ee"))
   :accents
   `(("yellow" . "#dbc074")
     ("orange" . "#f4a261")
     ("red" . "#c94f6d")
     ("magenta" . "#d67ad2")
     ("violet" . "#9d79d6")
     ("blue" . "#719cd6")
     ("cyan" . "#63cdcf")
     ("green" . "#81b29a"))))

;; Dayfox.
(defconst colorlab-dayfox
  (list
   :base
   `(("base03" . "#cdcecf")
     ("base02" . "#cdcecf")
     ("base01" . "#302b5d")
     ("base00" . "#3d2b5a")
     ("base0" . "#837a72")
     ("base1" . "#837a72")
     ("base2" . "#e4dcd4")
     ("base3" . "#f6f2ee"))
   :accents
   `(("yellow" . "#ac5402")
     ("orange" . "#955f61")
     ("red" . "#a5222f")
     ("magenta" . "#a440b5")
     ("violet" . "#6e33ce")
     ("blue" . "#2848a9")
     ("cyan" . "#287980")
     ("green" . "#396847"))))

;; Dawnfox.
(defconst colorlab-dawnfox
  (list
   :base
   `(("base03" . "#e0def4")
     ("base02" . "#e0def4")
     ("base01" . "#4c4769")
     ("base00" . "#575279")
     ("base0"  . "#9893a5")
     ("base1"  . "#9893a5")
     ("base2"  . "#ebe5df")
     ("base3"  . "#faf4ed"))
   :accents
   `(("yellow"  . ,(colorlab-modify-oklch "#ea9d34" 0.64 0.11))  ;; toned down to align with other accents
     ("orange"  . "#d7827e")
     ("red"     . "#b4637a")
     ("magenta" . "#d685af")
     ("violet"  . "#907aa9")
     ("blue"    . "#286983")
     ("cyan"    . "#56949f")
     ("green"   . "#618774"))))

;; Duskfox. Base2 and Base3 are dawnfox's backgrounds.
(defconst colorlab-duskfox
  (list
   :base
   `(("base03" . "#232136")
     ("base02" . "#2d2a45")
     ("base01" . "#817c9c")
     ("base00" . "#817c9c")
     ("base0"  . "#e0def4")
     ("base1"  . "#eae8ff")
     ("base2"  . "#ebe5df")
     ("base3"  . "#faf4ed"))
   :accents
   `(("yellow"  . "#f6c177")
     ("orange"  . "#ea9a97")
     ("red"     . "#eb6f92")
     ("magenta" . "#eb98c3")
     ("violet"  . "#c4a7e7")
     ("blue"    . "#569fba")
     ("cyan"    . "#9ccfd8")
     ("green"   . "#a3be8c"))))

;; Nordfox. Base2 and Base3 are its white and bright white.
(defconst colorlab-nordfox
  (list
   :base
   `(("base03" . "#2e3440")
     ("base02" . "#39404f")
     ("base01" . "#60728a")
     ("base00" . "#60728a")
     ("base0"  . "#cdcecf")
     ("base1"  . "#e5e9f0")  ;; its white (fg0 has less contrast than fg1)
     ("base2"  . "#e5e9f0")
     ("base3"  . "#e7ecf4"))
   :accents
   `(("yellow"  . "#ebcb8b")
     ("orange"  . "#c9826b")
     ("red"     . "#bf616a")
     ("magenta" . "#bf88bc")
     ("violet"  . ,(colorlab-set-oklch-hue "#b48ead" 300.0))  ;; purple turned toward blue
     ("blue"    . "#81a1c1")
     ("cyan"    . "#88c0d0")
     ("green"   . "#a3be8c"))))

;; Terafox. Base2 and Base3 are its white and bright white.
(defconst colorlab-terafox
  (list
   :base
   `(("base03" . "#152528")
     ("base02" . "#1d3337")
     ("base01" . "#6d7f8b")
     ("base00" . "#6d7f8b")
     ("base0"  . "#e6eaea")
     ("base1"  . "#eaeeee")
     ("base2"  . "#ebebeb")
     ("base3"  . "#eeeeee"))
   :accents
   `(("yellow"  . "#fda47f")
     ("orange"  . "#ff8349")
     ("red"     . "#e85c51")
     ("magenta" . "#ad5c7c")
     ("violet"  . ,(colorlab-set-oklch-hue "#ad5c7c" 300.0))  ;; magenta turned toward blue (no native purple)
     ("blue"    . "#5a93aa")
     ("cyan"    . "#a1cdd8")
     ("green"   . "#7aa4a1"))))

;; Carbonfox. Base2 and Base3 are its white and bright white; bg and fg are
;; the values upstream computes.
(defconst colorlab-carbonfox
  (list
   :base
   `(("base03" . "#161616")
     ("base02" . "#252525")
     ("base01" . "#6e6f70")
     ("base00" . "#6e6f70")
     ("base0"  . "#f2f4f8")
     ("base1"  . "#f9fbff")
     ("base2"  . "#dfdfe0")
     ("base3"  . "#e4e4e5"))
   :accents
   `(("yellow"  . "#f1c21b")  ;; IBM Carbon yellow 30 (upstream has teal)
     ("orange"  . "#ff832b")  ;; IBM Carbon orange 40 (upstream has teal)
     ("red"     . "#fa4d56")  ;; IBM Carbon red 50 (upstream has magenta)
     ("magenta" . "#ff7eb6")
     ("violet"  . "#be95ff")
     ("blue"    . "#78a9ff")
     ("cyan"    . "#08bdba")  ;; its teal (upstream yellow, cyan is close to blue)
     ("green"   . "#25be6a"))))

;; Carbon dark (g100): grade 40 accents, gray 60 comments. Base2 and Base3
;; are the white theme's backgrounds.
(defconst colorlab-carbon-dark
  (list
   :base
   `(("base03" . "#161616")
     ("base02" . "#262626")
     ("base01" . "#6f6f6f")
     ("base00" . "#6f6f6f")
     ("base0"  . "#c6c6c6")
     ("base1"  . "#f4f4f4")
     ("base2"  . "#f4f4f4")
     ("base3"  . "#ffffff"))
   :accents
   `(("yellow"  . "#d2a106")
     ("orange"  . "#ff832b")
     ("red"     . "#fa4d56")  ;; red 50 (Carbon's support-error) instead of red 40, to distinguish from magenta
     ("magenta" . "#ff7eb6")
     ("violet"  . "#be95ff")
     ("blue"    . "#78a9ff")
     ("cyan"    . "#08bdba")
     ("green"   . "#42be65"))))

;; Carbon light (white): grade 60 accents, gray 50 comments.
(defconst colorlab-carbon-light
  (list
   :base
   `(("base03" . "#c6c6c6")
     ("base02" . "#c6c6c6")
     ("base01" . "#161616")
     ("base00" . "#525252")
     ("base0"  . "#8d8d8d")
     ("base1"  . "#8d8d8d")
     ("base2"  . "#f4f4f4")
     ("base3"  . "#ffffff"))
   :accents
   `(("yellow"  . "#8e6a00")
     ("orange"  . "#ba4e00")
     ("red"     . "#da1e28")
     ("magenta" . "#d02670")
     ("violet"  . "#8a3ffc")
     ("blue"    . "#0072c3")  ;; cyan 60 (blue 60 is far more saturated)
     ("cyan"    . "#007d79")
     ("green"   . "#198038"))))

;; Apple dark: system colors. Base2 and Base3 are the light backgrounds.
(defconst colorlab-apple-dark
  (list
   :base
   `(("base03" . "#1c1c1e")
     ("base02" . "#2c2c2e")
     ("base01" . "#8e8e93")
     ("base00" . "#8e8e93")
     ("base0"  . "#ebebf5")
     ("base1"  . "#ffffff")
     ("base2"  . "#f2f2f7")
     ("base3"  . "#ffffff"))
   :accents
   `(("yellow"  . "#ffd600")
     ("orange"  . "#ff9230")
     ("red"     . "#ff4245")
     ("magenta" . "#db34f2")  ;; systemPurple (pink is close to red)
     ("violet"  . "#6d7cff")  ;; systemIndigo
     ("blue"    . "#0091ff")
     ("cyan"    . "#00dac3")  ;; systemMint (teal/cyan is close to blue)
     ("green"   . "#30d158"))))

;; Apple light: the increased contrast colors, as the system yellow, orange,
;; green and mint are under 2.4:1 on white.
(defconst colorlab-apple-light
  (list
   :base
   `(("base03" . "#ebebf5")
     ("base02" . "#ebebf5")
     ("base01" . "#000000")
     ("base00" . "#3c3c43")
     ("base0"  . "#8e8e93")
     ("base1"  . "#8e8e93")
     ("base2"  . "#f2f2f7")
     ("base3"  . "#ffffff"))
   :accents
   `(("yellow"  . "#a16a00")
     ("orange"  . "#c55300")
     ("red"     . "#e9152d")
     ("magenta" . "#b02fc2")  ;; systemPurple (pink is close to red)
     ("violet"  . "#564ade")  ;; systemIndigo
     ("blue"    . "#1e6ef4")
     ("cyan"    . "#008575")  ;; systemMint (teal/cyan is close to blue)
     ("green"   . "#008932"))))

(defvar colorlab-palettes
  (list
   (list :name "solarized-dark"
         :solarized colorlab-solarized
         :accent1 (list :bg-amt 0.85
                        :fg-amt 0.30
                        :gamma 0.01
                        :min-diff 0.40
                        :fg-finder "base1"
                        :bg-finder 'extreme-bg)
         :accent2 (list :bg-amt 0.60
                        :fg-amt 0.45
                        :gamma 0.04
                        :min-diff 0.40
                        :fg-finder "base1"
                        :bg-finder 'extreme-bg))
   (list :name "solarized-light"
         :solarized colorlab-solarized
         :inverse t)
   (list :name "solarized-dark-high-contrast"
         :solarized colorlab-solarized-dark-high-contrast
         :accent1 (list :bg-amt 0.85
                        :fg-amt 0.30
                        :gamma 0.01
                        :min-diff 0.40
                        :fg-finder "base1"
                        :bg-finder 'extreme-bg)
         :accent2 (list :bg-amt 0.60
                        :fg-amt 0.45
                        :gamma 0.04
                        :min-diff 0.40
                        :fg-finder "base1"
                        :bg-finder 'extreme-bg))
   (list :name "solarized-light-high-contrast"
         :solarized colorlab-solarized-light-high-contrast
         :inverse t)
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
         :solarized colorlab-selenized-dark)
   (list :name "selenized-black"
         :solarized colorlab-selenized-black)
   (list :name "selenized-light"
         :solarized colorlab-selenized-light
         :inverse t)
   (list :name "selenized-white"
         :solarized colorlab-selenized-white
         :inverse t)
   (list :name "everforest-dark"
         :solarized colorlab-everforest-dark)
   (list :name "everforest-light"
         :solarized colorlab-everforest-light
         :inverse t)
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
         :inverse t)
   (list :name "nightfox"
         :solarized colorlab-nightfox)
   (list :name "dayfox"
         :solarized colorlab-dayfox
         :inverse t)
   (list :name "dawnfox"
         :solarized colorlab-dawnfox
         :inverse t)
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
         :inverse t))
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
    (if no-update
        (progn
          (dolist (pal palettes)
            (let* ((name (plist-get pal :name))
                   (nc (colorlab-generate-palette-alist pal)))
              (princ (format "%s\n" name))
              (princ (colorlab-format-alist nc 0))))
          (princ "\n-----\n\n"))
      (with-temp-buffer
        (insert-file-contents target-file)
        (dolist (pal palettes)
          (let* ((name (plist-get pal :name))
                 (nc (colorlab-generate-palette-alist pal))
                 (marker-re (format ";;[ \t]*%s\\(?:[ \t]+color\\)?[ \t]+palette[ \t]*$" (regexp-quote name))))
            (princ (format "%s\n" name))
            (princ (colorlab-format-alist nc 0))
            (goto-char (point-min))
            (when (re-search-forward marker-re nil t)
              (forward-line 1)
              (let ((start (point)))
                (if (re-search-forward "^[ \t]*;;[ \t]*palette end[ \t]*$" nil t)
                    (progn
                      (beginning-of-line)
                      (delete-region start (point))
                      (insert (colorlab-format-alist nc 4)))
                  (message "Warning: palette end marker not found for %s" name))))))
        (write-region (point-min) (point-max) target-file nil 'quiet)
        (princ "\n-----\n\n")
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
