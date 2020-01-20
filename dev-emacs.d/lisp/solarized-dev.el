(defun dev-set-solarized-settings ()
  (interactive)
  (setq-default
   tooltip-delay 0.8
   ;;tooltip-hide-delay 10
   ;;tooltip-recent-seconds 1
   x-gtk-use-system-tooltips nil

   solarized-distinct-fringe-background nil
   solarized-distinct-doc-face nil
   solarized-use-variable-pitch t
   solarized-use-less-bold t
   solarized-use-more-italic t
   solarized-emphasize-indicators t
   solarized-high-contrast-mode-line nil
   solarized-height-minus-1 0.8
   solarized-height-plus-1 1.1
   solarized-height-plus-2 1.15
   solarized-height-plus-3 1.2
   solarized-height-plus-4 1.3
   solarized-scale-org-headlines t
   solarized-scale-outline-headlines t
   ;; end
   ))

(defun dev-setup-paths (dev-init-dir)
  (let* ((init-dir dev-init-dir)
         (elisp-dir (expand-file-name ".." init-dir))
         (childtheme-dir (expand-file-name "../child-theme-example" init-dir))
         (dev-lisp-dir (expand-file-name "lisp" init-dir))
         (childtheme-themes-dir (expand-file-name "themes" childtheme-dir)))
    (defvar dev-project-root elisp-dir)
    (add-to-list 'load-path elisp-dir)
    (add-to-list 'load-path childtheme-dir)
    (add-to-list 'load-path dev-lisp-dir)
    (setq custom-theme-load-path (list elisp-dir childtheme-themes-dir)
          package-user-dir (expand-file-name "elpa" init-dir)
          package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                             ("melpa" . "https://melpa.org/packages/")))))
(defun dev-themes ()
  "get list of all themes"
  (let ((themes nil))
    (setq themes (custom-available-themes))
    (setq themes (remove 'solarized themes))
    ;;(setq themes (cl-sort themes 'string-lessp :key '(lambda (v) (downcase (symbol-name v)))))
    themes))


(defun dev-cycle-themes (&optional themes)
  (interactive)
  (let* ((themes (or themes (dev-themes)))
         (current-theme-candidate (and custom-enabled-themes (car custom-enabled-themes)))
         (current-theme (if (member current-theme-candidate themes)
                            current-theme-candidate
                          (car themes)))
         (next-theme (if (not current-theme)
                         dev-default-theme
                       (nth (mod (+ 1 (seq-position themes current-theme))
                                 (length themes))
                            themes))))
    (dev-disable-all-themes)
    (message "loading theme: %s" next-theme)
    (load-theme next-theme t)))


(defun dev-cycle-main-themes ()
  (interactive)
  (dev-cycle-themes '(solarized-dark solarized-light)))


(defun dev-disable-all-themes()
  (interactive)
  (dolist (v custom-enabled-themes)
    (disable-theme v)))


(defvar dev-reload-theme-last-setting nil)
(defun dev-reload-theme()
  (interactive)
  (unless dev-reload-theme-last-setting
    (setq dev-reload-theme-last-setting (copy-sequence custom-enabled-themes)))
  (dev-save-elisp-buffers)
  (ignore-errors
    (unload-feature 'solarized t))
  (ignore-errors
    (unload-feature 'solarized-faces t))
  (ignore-errors
    (unload-feature 'solarized-palettes t))
  (dev-set-solarized-settings)
  (load-library "solarized")
  (let ((themes (reverse (or dev-reload-theme-last-setting
                             custom-enabled-themes))))
    (dev-disable-all-themes)
    (dolist (v themes)
      (load-theme v t)))
  (setq dev-reload-theme-last-setting nil))


(defun dev-save-elisp-buffers ()
  (interactive)
  (save-some-buffers 'no-confirm
                     (lambda ()
                       (and buffer-file-name
                            (eq major-mode 'emacs-lisp-mode)
                            (string-prefix-p dev-project-root buffer-file-name)))))

(defun dev-switch-buffer-solarized ()
  (interactive)
  (switch-to-buffer (get-buffer "solarized.el")))


(defun dev-switch-buffer-current-theme ()
  (interactive)
  (dolist
      (p custom-theme-load-path)
    (let* (
           (current-theme (and custom-enabled-themes (car custom-enabled-themes)))
           (current-theme-filename (expand-file-name (concat (symbol-name current-theme) "-theme.el") p)))
      (if (file-exists-p current-theme-filename)
          (find-file current-theme-filename)))))


(defvar dev-tf-current-buffer nil)
(defun dev-tf-switch-buffer (&optional backwards)
  (let* ((cur (current-buffer))
         (all (seq-filter '(lambda (b)
                             (let ((filen (buffer-file-name b)))
                               (and
                                filen
                                (string-suffix-p "/test-files/" (file-name-directory filen)))))
                          (buffer-list)))

         (all-len  (length all))
         (all-sorted (cl-sort all 'string-lessp :key '(lambda (b) (downcase (buffer-file-name b)))))
         (cur-idx-pos (seq-position all-sorted cur))
         (next-idx-pos (cond
                        ((not cur-idx-pos) 0)
                        ((and backwards (= cur-idx-pos 0) ) (- all-len 1))
                        ((and (not backwards) (= cur-idx-pos (- all-len 1))) 0)
                        (t (+ cur-idx-pos (if backwards -1 1)))))
         (next-buf (if (and (not cur-idx-pos)
                            (bufferp dev-tf-current-buffer)
                            (buffer-live-p dev-tf-current-buffer))
                       dev-tf-current-buffer
                     (elt all-sorted next-idx-pos))))

    (setq dev-tf-current-buffer next-buf)
    (switch-to-buffer next-buf)))


(defun dev-tf-switch-buffer-next ()
  (interactive)
  (dev-tf-switch-buffer))


(defun dev-tf-switch-buffer-prev ()
  (interactive)
  (dev-tf-switch-buffer t))


(provide 'solarized-dev)
