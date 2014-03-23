;; things that generate or filter colors
;;; for now just some stuff that might not be there later, I belive these are copied or adapted from emacswiki.

;;;; randomize buffer background
(defun randomize-buffer-background ()
  "changes current buffer's background to a random color (close to the defualt of this face)"
  (interactive)
  (progn
    (setq face-symbol (gensym "face-"))
    (make-face face-symbol)
    (buffer-face-set face-symbol)
    (setq rgb (mapcar
               (function
                (lambda (x) (let ((y (* 0.95 (+ x (/ (- (random 100) 50) 1200.0)))))
                         (if (> y 1) (- 2 y) (if (< y 0) (- y) y)))))
               (color-name-to-rgb (face-background 'default))))
    (setq new-color (color-rgb-to-hex (car rgb) (car (cdr rgb)) (car (cdr (cdr rgb)))))
    (set-face-background face-symbol new-color) (message (concat "color changed to " new-color))))

;;; functions: wash out faces
(defun wash-out-colour (colour &optional degree)
  "Return a colour string specifying a washed-out version of COLOUR."
  (let ((basec (color-values
                (face-attribute 'default :foreground)))
        (col (color-values colour))
        (list nil))
    (unless degree (setq degree 2))
    (while col
      (push (/ (/ (+ (pop col)
                     (* degree (pop basec)))
                  (1+ degree))
               256)
            list))
    (apply 'format "#%02x%02x%02x" (nreverse list))))

(defun wash-out-face (face &optional degree bg)
  "Make the foreground colour of FACE appear a bit more pale."
  (let* ((prop (if bg :background :foreground))
         (colour (face-attribute face prop)))
    (unless (eq colour 'unspecified)
      (set-face-attribute face nil
                          prop (wash-out-colour colour degree)))))

(defun find-faces (regexp)
  "Return a list of all faces whose names match REGEXP."
  (delq nil
        (mapcar (lambda (face)
                  (and (string-match regexp
                                   (symbol-name face))
                     face))
                (face-list))))

(defun wash-out-fontlock-faces (&optional degree)
  (--each (face-list)
    (let ((name (symbol-name it)))
      (cond
       ((s-match "^font-lock-comment" name) t)
       ((s-match "^isearch" name)
        (wash-out-face it (/ degree 4) t))
       ((s-match "^ahs-\\\|^sp-show-pair-.*match\\\|^font-lock-warning-face\\\|^anzu" name)
        (wash-out-face it (/ degree 4)))
       ((s-match "^font-lock\\\|^org-\\\|^webmode" name)
        (wash-out-face it degree))))))


(defun wash-out-faces ()
  (interactive)
  (wash-out-fontlock-faces 0.2))



