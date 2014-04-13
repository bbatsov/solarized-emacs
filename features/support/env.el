(require 'f)

(defvar solarized-emacs-support-path
  (f-dirname load-file-name))

(defvar solarized-emacs-features-path
  (f-parent solarized-emacs-support-path))

(defvar solarized-emacs-root-path
  (f-parent solarized-emacs-features-path))

(add-to-list 'load-path solarized-emacs-root-path)

;; TODO Dont know if this is needed
(if (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path solarized-emacs-root-path)
  (add-to-list 'load-path solarized-emacs-root-path))

(require 'solarized-theme) 
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
