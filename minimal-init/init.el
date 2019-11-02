
(let* ((init-dir (file-name-as-directory (file-name-directory load-file-name)))
       (elisp-dir (expand-file-name ".." init-dir)))

  (add-to-list 'load-path elisp-dir)
  (setq custom-theme-load-path (list elisp-dir)
        package-user-dir (expand-file-name "elpa" init-dir)
        package-archives '(("gnu" . "https://elpa.gnu.org/packages/") ("melpa" . "https://melpa.org/packages/"))))


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
      '(dash flx-ido projectile web-mode haskell-mode go-mode js2-mode markdown-mode))

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


(load-theme 'solarized-dark)
(load-theme 'solarized-light)
