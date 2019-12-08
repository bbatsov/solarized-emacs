;; this init file is primarily focused around developing and
;; evaluating theming outside of ones own emacs.d.

;; you can run like 'emacs -q -l {{pkg-dir}}/dev-emacs.d/init.el'
(when load-file-name
  (setq user-emacs-directory
        (expand-file-name (file-name-directory load-file-name))))

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

(setq pop-up-frame-function nil
      inhibit-startup-screen t
      initial-scratch-message nil
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      display-buffer-alist
      '((".*" (display-buffer-reuse-window
               display-buffer-same-window)))
      enable-local-variables nil
      web-mode-enable-engine-detection t
      ediff-make-buffers-readonly-at-startup nil
      ediff-diff-options ""
      magit-diff-refine-hunk 'all
      magit-diff-refine-ignore-whitespace nil
      ediff-window-setup-function 'ediff-setup-windows-plain
      ;;
      )

(require 'subr-x)

(defun dev-debug-make-frame ()
  ;; debug-on-entry on make-frame does not work
  (define-advice make-frame (:around (fn &rest args) suppress)
    "Suppress making new frame; return existing frame."
    (debug)
    (message "make-frame suppressed; proceed at your own peril.")
    (selected-frame)))
;; (debug-make-frame)


(when load-file-name
  (let* ((init-dir (file-name-as-directory (file-name-directory load-file-name)))
         (elisp-dir (expand-file-name ".." init-dir))
         (childtheme-dir (expand-file-name "../solarized-child-themes" init-dir))
         (childtheme-sample-dir (expand-file-name "../child-theme-example" init-dir))
         (childtheme-sample-themes-dir (expand-file-name "themes" childtheme-sample-dir)))

    (defvar dev-project-root elisp-dir)

    (add-to-list 'load-path elisp-dir)
    (add-to-list 'load-path childtheme-sample-dir)
    (setq custom-theme-load-path (list elisp-dir childtheme-dir childtheme-sample-themes-dir)
          package-user-dir (expand-file-name "elpa" init-dir)
          package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                             ("melpa" . "https://melpa.org/packages/")))))
(defun dev-open-merge()
  (interactive)
  (find-file (expand-file-name "dev-emacs.d/test-repo/" dev-project-root)))

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
(global-set-key (kbd "<f7>") 'dev-cycle-themes)

(defun dev-cycle-main-themes ()
  (interactive)
  (dev-cycle-themes '(solarized-dark solarized-light)))

(global-set-key (kbd "<f6>") 'dev-cycle-main-themes)
(global-set-key (kbd "<f8>") 'load-theme)

(defun dev-disable-all-themes()
  (interactive)
  (dolist (v custom-enabled-themes)
    (disable-theme v)))
(global-set-key (kbd "<f4>") 'dev-disable-all-themes)

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
  (dev-set-solarized-settings)
  (load-library "solarized")
  (let ((themes (reverse (or dev-reload-theme-last-setting
                             custom-enabled-themes))))
    (dev-disable-all-themes)
    (dolist (v themes)
      (load-theme v t)))
  (setq dev-reload-theme-last-setting nil))
(global-set-key (kbd "<f5>") 'dev-reload-theme)

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
(global-set-key (kbd "<f9>") ' dev-switch-buffer-solarized)

(defun dev-switch-buffer-current-theme ()
  (interactive)
  (dolist
      (p custom-theme-load-path)
    (let* (
           (current-theme (and custom-enabled-themes (car custom-enabled-themes)))
           (current-theme-filename (expand-file-name (concat (symbol-name current-theme) "-theme.el") p)))
      (if (file-exists-p current-theme-filename)
          (find-file current-theme-filename)))))
(global-set-key (kbd "<f10>") 'dev-switch-buffer-current-theme)

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
(global-set-key (kbd "<f11>") 'dev-tf-switch-buffer-next)

(defun dev-tf-switch-buffer-prev ()
  (interactive)
  (dev-tf-switch-buffer t))
(global-set-key (kbd "<f12>") 'dev-tf-switch-buffer-prev)



(dev-set-solarized-settings)

(require 'package)
(package-initialize)

(defun require-package (package &optional min-version no-refresh)
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(mapc 'require-package
      '(;; required by solarized
        dash
        ;; utility/convenience
        flx-ido projectile
        ;; utility/convenience + face testing
        magit smartparens htmlize
        ;; editor major modes for face testing
        web-mode haskell-mode go-mode js2-mode markdown-mode rust-mode php-mode))

(setq auto-mode-alist
      (append '(("\\.webmode\\'" . web-mode))
       auto-mode-alist))

(when load-file-name
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  (smartparens-global-strict-mode)
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  (projectile-global-mode)
  (global-set-key (kbd "C-h h") 'ibuffer)
  (global-set-key (kbd "C-h g") 'magit))


(setq
 projectile-sort-order 'recently-active
 projectile-completion-system 'ido
 projectile-switch-project-action 'projectile-dired
 projectile-verbose nil)

(when load-file-name
  (dev-disable-all-themes)
  (load-theme 'my-solarized-dark t)
  (dev-disable-all-themes)
  (load-theme 'my-solarized-light t)
  (dev-disable-all-themes)
  (load-theme 'solarized-dark t)
  (dev-disable-all-themes)
  (load-theme 'solarized-light t))
