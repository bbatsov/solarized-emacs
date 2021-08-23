;; this init file is primarily focused around developing and
;; evaluating theming outside of ones own emacs.d.

;; you can run like 'emacs -q -l {{pkg-dir}}/dev-emacs.d/init.el'
(when load-file-name
  (setq user-emacs-directory
        (expand-file-name (file-name-directory load-file-name))))

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

(when load-file-name
  (add-to-list 'load-path
               (expand-file-name
                "lisp"
                (file-name-as-directory
                 (file-name-directory load-file-name)))))

(require 'solarized-dev)
(require 'solarized-dev-keys)

(when load-file-name
  (dev-setup-paths (file-name-as-directory
                    (file-name-directory load-file-name))))

(defun dev-debug-make-frame ()
  ;; debug-on-entry on make-frame does not work
  (define-advice make-frame (:around (fn &rest args) suppress)
    "Suppress making new frame; return existing frame."
    (debug)
    (message "make-frame suppressed; proceed at your own peril.")
    (selected-frame)))
;; (dev-debug-make-frame)
;; (toggle-debug-on-error)



(defun dev-open-merge()
  (interactive)
  (find-file (expand-file-name "dev-emacs.d/test-repo/" dev-project-root)))

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
      '(;; utility/convenience
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

(dev-set-solarized-settings)

(when load-file-name
  (dev-disable-all-themes)
  (load-theme 'my-solarized-dark t)
  (dev-disable-all-themes)
  (load-theme 'my-solarized-light t)
  (dev-disable-all-themes)
  (load-theme 'solarized-dark t)
  (dev-disable-all-themes)
  (load-theme 'solarized-light t))
