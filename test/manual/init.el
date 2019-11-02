(let* ((init-dir (file-name-as-directory (file-name-directory load-file-name)))
       (elisp-dir (expand-file-name "../../" init-dir))
       (childtheme-dir (expand-file-name "../child-theme-example" init-dir))
       (childtheme-themes-dir (expand-file-name "themes" childtheme-dir))
       )

  (add-to-list 'load-path elisp-dir)
  (add-to-list 'load-path childtheme-dir)
  (setq custom-theme-load-path (list elisp-dir childtheme-themes-dir)
        package-user-dir (expand-file-name "elpa" init-dir)
        package-archives '(("gnu" . "https://elpa.gnu.org/packages/") ("melpa" . "https://melpa.org/packages/"))))

(setq safe-local-variable-values '((eval when (fboundp 'rainbow-mode) (rainbow-mode 1))))

(setq
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
)

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
      '(dash flx-ido projectile web-mode haskell-mode go-mode js2-mode markdown-mode magit smartparens))

(smartparens-global-mode)
(show-smartparens-global-mode)

(global-set-key (kbd "C-h h") 'ibuffer)
(global-set-key (kbd "C-h g") 'magit)

(setq
 projectile-sort-order 'recently-active
 projectile-completion-system 'ido
 projectile-switch-project-action 'projectile-dired
 projectile-verbose nil)

(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

(projectile-global-mode)

(require 'solarized)

(load-theme 'my-solarized-dark t)
(load-theme 'my-solarized-light t)
(load-theme 'solarized-dark t)
(load-theme 'solarized-light t)
