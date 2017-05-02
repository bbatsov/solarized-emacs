;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(When "^I load the theme \"\\(.+\\)\"$"
  "Loads and theme."
  (lambda (theme)
    (load-theme theme t)))

(defun steps-face-background-color-p (colorname)
  "Make sure the character at point is bold."
  (espuds-character-fontified-p
   :background (list colorname)))

(Then "^current point should have background color \"\\(.+\\)\"$"
  (lambda (color)
    (cl-assert
     (steps-face-background-color-p color)
     nil
     (format "Expected current point to have background color %s" color))))


;; (Given "^I have \"\\(.+\\)\"$"
;;   (lambda (something)
;;     ;; ...
;;     ))

;; (When "^I have \"\\(.+\\)\"$"
;;   (lambda (something)
;;     ;; ...
;;     ))

;; (Then "^I should have \"\\(.+\\)\"$"
;;   (lambda (something)
;;     ;; ...
;;     ))

;; (And "^I have \"\\(.+\\)\"$"
;;   (lambda (something)
;;     ;; ...
;;     ))

;; (But "^I should not have \"\\(.+\\)\"$"
;;   (lambda (something)
;;     ;; ...
;;     ))
