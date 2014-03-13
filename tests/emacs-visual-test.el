;;; emacs-visual-test.el ---

;;; Commentary:
;;

(load-file "init.el")
(require 's)
(require 'dash)
(require 'f)
(setq header-line-format "header line content")

(defconst screenshots-directory
  (f-expand "screenshots" tests-directory))

(defconst test-files-directory
  (f-expand "test-files" tests-directory))

(defun visual-test-find-file (  name)
  (find-file (f-expand name test-files-directory)))

(defun visual-test-screenshot ()
  (call-process "scrot" nil nil nil "-u"
                (f-expand "screenshot-%Y-%m-%d_%H-%M-%S_$wx$h.png"
                          screenshots-directory))
  (message "saved screenshot"))

;; (setq solarized-high-contrast-mode-line t)
;; (load-theme 'solarized-dark t)
(load-theme 'solarized-light t)
(magit-status default-directory)
(split-window-right)
(visual-test-find-file "django-template.html")
(setq header-line-format "header line content")
(web-mode)
(goto-char 27)
(call-interactively 'ispell-complete-word)


(run-with-idle-timer 2 nil
                     '(lambda ()
                        (visual-test-screenshot)
                        ;; (keyboard-quit)
                        ;; (kill-buffer)
                        ;; (kill-emacs)
                        ))



(provide 'emacs-visual-test)

;;; emacs-visual-test.el ends here
