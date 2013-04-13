;;; solarized.el --- Solarized for Emacs.

;; Copyright (C) 2011-2013 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; Author: Thomas Frössman <thomasf@jossystem.se>
;; URL: http://github.com/bbatsov/solarized-emacs
;; Version: 1.0.0

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
;;
;; A port of Solarized to Emacs.
;;
;;; Installation:
;;
;;   Drop the `solarized-theme.el` somewhere in your `load-path` and
;; the two themes in a folder that is on `custom-theme-load-path'
;; and enjoy!
;;
;; Don't forget that the theme requires Emacs 24.
;;
;;; Bugs
;;
;; None that I'm aware of.
;;
;;; Credits
;;
;; Ethan Schoonover created the original theme for vim on such this port
;; is based.
;;
;;; Code:

(defun create-solarized-theme (variant theme-name &optional childtheme)
  (let* ((class '((class color) (min-colors 89)))
         ;; Solarized palette
         (base03    "#002b36")
         (base02    "#073642")
         ;; emphasized content
         (base01    "#586e75")
         ;; primary content
         (base00    "#657b83")
         (base0     "#839496")
         ;; comments
         (base1     "#93a1a1")
         ;; background highlight light
         (base2     "#eee8d5")
         ;; background light
         (base3     "#fdf6e3")

         ;; Solarized accented colors
         (yellow    "#b58900")
         (orange    "#cb4b16")
         (red       "#dc322f")
         (magenta   "#d33682")
         (violet    "#6c71c4")
         (blue      "#268bd2")
         (cyan      "#2aa198")
         (green     "#859900")

         ;; Darker and lighter accented colors
         ;; Only use these in exceptional circumstances!
         (yellow-d  "#7B6000")
         (yellow-l  "#DEB542")
         (orange-d  "#8B2C02")
         (orange-l  "#F2804F")
         (red-d     "#990A1B")
         (red-l     "#FF6E64")
         (magenta-d "#93115C")
         (magenta-l "#F771AC")
         (violet-d  "#3F4D91")
         (violet-l  "#9EA0E5")
         (blue-d    "#00629D")
         (blue-l    "#69B7F0")
         (cyan-d    "#00736F")
         (cyan-l    "#69CABF")
         (green-d   "#546E00")
         (green-l   "#B4C342")

         ;; Light/Dark adaptive solarized colors
         (solarized-fg (if (eq variant 'light) base00 base0))
         (solarized-bg (if (eq variant 'light) base3 base03))
         (solarized-hl (if (eq variant 'light) base2 base02))
         (solarized-emph (if (eq variant 'light) base01 base1))
         (solarized-comments (if (eq variant 'light) base1 base01))

         ;; Light/Dark adaptive higher/lower contrast accented colors
         ;; Only use these in exceptional cirmumstances!
         (solarized-fg-hc (if (eq variant 'light) base3 base03))
         (solarized-fg-lc (if (eq variant 'light) base03 base3))

         (yellow-hc (if (eq variant 'light) yellow-d yellow-l))
         (yellow-lc (if (eq variant 'light) yellow-l yellow-d))
         (orange-hc (if (eq variant 'light) orange-d orange-l))
         (orange-lc (if (eq variant 'light) orange-l orange-d))
         (red-hc (if (eq variant 'light) red-d red-l))
         (red-lc (if (eq variant 'light) red-l red-d))
         (magenta-hc (if (eq variant 'light) magenta-d magenta-l))
         (magenta-lc (if (eq variant 'light) magenta-l magenta-d))
         (violet-hc (if (eq variant 'light) violet-d violet-l))
         (violet-lc (if (eq variant 'light) violet-l violet-d))
         (blue-hc (if (eq variant 'light) blue-d blue-l))
         (blue-lc (if (eq variant 'light) blue-l blue-d))
         (cyan-hc (if (eq variant 'light) cyan-d cyan-l))
         (cyan-lc (if (eq variant 'light) cyan-l cyan-d))
         (green-hc (if (eq variant 'light) green-d green-l))
         (green-lc (if (eq variant 'light) green-l green-d)))
    (custom-theme-set-faces
     theme-name
     '(button ((t (:underline t))))

     ;; basic coloring
     `(default ((,class (:foreground ,solarized-fg :background ,solarized-bg))))
     `(shadow ((,class (:foreground ,solarized-comments))))
     `(match ((,class (:background ,solarized-hl :foreground ,solarized-emph :weight bold))))
     `(cursor ((,class (:foreground ,solarized-bg :background ,solarized-fg :inverse-video t))))
     `(escape-glyph-face ((,class (:foreground ,red))))
     `(fringe ((,class (:foreground ,solarized-fg :background ,solarized-hl))))
     `(header-line ((,class (:foreground ,yellow
                                         :background ,solarized-hl
                                         :box (:line-width -1 :style released-button)))))
     `(highlight ((,class (:background ,solarized-hl))))
     `(link ((,class (:foreground ,yellow :underline t :weight bold))))
     `(link-visited ((,class (:foreground ,yellow :underline t :weight normal))))
     `(success ((,class (:foreground ,green ))))
     `(warning ((,class (:foreground ,yellow ))))
     `(error ((,class (:foreground ,orange))))
     `(lazy-highlight ((,class (:foreground ,solarized-emph :background ,solarized-hl :bold t))))
     `(escape-glyph ((,class (:foreground ,violet))))

     ;; compilation
     `(compilation-column-face ((,class (:foreground ,yellow))))
     `(compilation-enter-directory-face ((,class (:foreground ,green))))
     `(compilation-error-face ((,class (:foreground ,red :weight bold :underline t))))
     `(compilation-face ((,class (:foreground ,solarized-fg))))
     `(compilation-info-face ((,class (:foreground ,blue))))
     `(compilation-info ((,class (:foreground ,green :underline t))))
     `(compilation-leave-directory-face ((,class (:foreground ,green))))
     `(compilation-line-face ((,class (:foreground ,yellow))))
     `(compilation-line-number ((,class (:foreground ,yellow))))
     `(compilation-message-face ((,class (:foreground ,blue))))
     `(compilation-warning-face ((,class (:foreground ,yellow :weight bold :underline t))))

     `(compilation-mode-line-exit
       ((,class (:inherit compilation-info :foreground ,green :weight bold))))
     `(compilation-mode-line-fail
       ((,class (:inherit compilation-error :foreground ,red :weight bold))))
     `(compilation-mode-line-run ((,class (:foreground ,orange :weight bold))))

     ;; cua
     `(cua-global-mark ((,class (:background ,yellow :foreground ,solarized-bg))))
     `(cua-rectangle ((,class (:inherit region :background ,magenta :foreground ,solarized-bg))))
     `(cua-rectangle-noselect ((,class (:inherit region :background ,solarized-hl
                                                 :foreground ,solarized-comments))))

     ;; diary
     `(diary ((,class (:foreground ,yellow))))

     ;; dired
     `(dired-directory ((,class (:foreground ,blue :weight normal))))
     `(dired-flagged ((,class (:foreground ,red))))
     `(dired-header ((,class (:foreground ,solarized-bg :background ,blue))))
     `(dired-ignored ((,class (:inherit shadow))))
     `(dired-mark ((,class (:foreground ,yellow :weight bold))))
     `(dired-marked ((,class (:foreground ,magenta :weight bold))))
     `(dired-perm-write ((,class (:foreground ,solarized-fg :underline t))))
     `(dired-symlink ((,class (:foreground ,cyan :weight normal :slant italic))))
     `(dired-warning ((,class (:foreground ,orange :underline t))))

     ;; dropdown
     `(dropdown-list-face ((,class (:background ,solarized-hl :foreground ,cyan))))
     `(dropdown-list-selection-face ((,class (:background ,cyan-lc :foreground ,cyan-hc))))

     ;; grep
     `(grep-context-face ((,class (:foreground ,solarized-fg))))
     `(grep-error-face ((,class (:foreground ,red :weight bold :underline t))))
     `(grep-hit-face ((,class (:foreground ,blue))))
     `(grep-match-face ((,class (:foreground ,orange :weight bold))))

     ;; faces used by isearch
     `(isearch ((,class (:foreground ,yellow :background ,solarized-hl :bold t))))
     `(isearch-fail ((,class (:foreground ,red :background ,solarized-bg :bold t))))

     ;; man
     `(Man-overstrike ((,class (:foreground ,blue :weight bold))))
     `(Man-reverse ((,class (:foreground ,orange))))
     `(Man-underline ((,class (:foreground ,green :underline t))))

     ;; misc faces
     `(menu ((,class (:foreground ,solarized-fg :background ,solarized-bg))))
     `(minibuffer-prompt ((,class (:foreground ,solarized-emph))))
     `(mode-line
       ((,class (:foreground ,solarized-fg
                             :background ,solarized-hl
                             :box (:line-width -1 :style released-button)))))
     `(mode-line-buffer-id ((,class (:foreground ,solarized-emph :weight bold))))
     `(mode-line-inactive
       ((,class (:foreground ,solarized-fg
                             :background ,solarized-bg
                             :box (:line-width -1 :style released-button)))))
     `(region ((,class (:foreground ,solarized-bg :background ,solarized-emph))))
     `(secondary-selection ((,class (:background ,solarized-hl))))

     `(trailing-whitespace ((,class (:background ,red))))
     `(vertical-border ((,class (:foreground ,solarized-fg))))

     ;; font lock
     `(font-lock-builtin-face ((,class (:foreground ,blue :slant italic))))
     `(font-lock-comment-delimiter-face ((,class (:foreground ,solarized-comments))))
     `(font-lock-comment-face ((,class (:foreground ,solarized-comments))))
     `(font-lock-constant-face ((,class (:foreground ,blue :weight bold))))
     `(font-lock-doc-face ((,class (:foreground ,cyan :slant italic))))
     `(font-lock-doc-string-face ((,class (:foreground ,blue))))
     `(font-lock-function-name-face ((,class (:foreground ,blue))))
     `(font-lock-keyword-face ((,class (:foreground ,green :weight bold))))
     `(font-lock-negation-char-face ((,class (:foreground ,solarized-fg))))
     `(font-lock-preprocessor-face ((,class (:foreground ,blue))))
     `(font-lock-string-face ((,class (:foreground ,cyan))))
     `(font-lock-type-face ((,class (:foreground ,yellow))))
     `(font-lock-variable-name-face ((,class (:foreground ,blue))))
     `(font-lock-warning-face ((,class (:foreground ,orange :weight bold :underline t))))

     `(c-annotation-face ((,class (:inherit font-lock-constant-face))))

     ;;; external

     ;; ace-jump-mode
     `(ace-jump-face-background
       ((,class (:foreground ,solarized-comments :background ,solarized-bg :inverse-video nil))))
     `(ace-jump-face-foreground
       ((,class (:foreground ,red :background ,solarized-bg :inverse-video nil))))

     ;; auto-complete
     `(ac-candidate-face ((,class (:background ,solarized-hl :foreground ,cyan))))
     `(ac-selection-face ((,class (:background ,cyan-lc :foreground ,cyan-hc))))
     `(ac-candidate-mouse-face ((,class (:background ,cyan-hc :foreground ,cyan-lc))))
     `(ac-completion-face ((,class (:foreground ,solarized-emph :underline t))))
     `(ac-gtags-candidate-face ((,class (:background ,solarized-hl :foreground ,blue))))
     `(ac-gtags-selection-face ((,class (:background ,blue-lc :foreground ,blue-hc))))
     `(ac-yasnippet-candidate-face ((,class (:background ,solarized-hl :foreground ,yellow))))
     `(ac-yasnippet-selection-face ((,class (:background ,yellow-lc :foreground ,yellow-hc))))

     ;; auto highlight symbol
     `(ahs-definition-face ((,class (:foreground ,solarized-bg :background ,blue :underline t))))
     `(ahs-edit-mode-face ((,class (:foreground ,solarized-bg :background ,yellow))))
     `(ahs-face ((,class (:foreground ,solarized-bg :background ,blue))))
     `(ahs-plugin-bod-face ((,class (:foreground ,solarized-bg :background ,blue))))
     `(ahs-plugin-defalt-face ((,class (:foreground ,solarized-bg :background ,cyan))))
     `(ahs-plugin-whole-buffer-face ((,class (:foreground ,solarized-bg :background ,green))))
     `(ahs-warning-face ((,class (:foreground ,red :weight bold))))

     ;; android mode
     `(android-mode-debug-face ((,class (:foreground ,green))))
     `(android-mode-error-face ((,class (:foreground ,orange :weight bold))))
     `(android-mode-info-face ((,class (:foreground ,solarized-fg))))
     `(android-mode-verbose-face ((,class (:foreground ,solarized-comments))))
     `(android-mode-warning-face ((,class (:foreground ,yellow))))

     ;; bm
     `(bm-face ((,class (:background ,yellow-lc :foreground ,solarized-bg))))
     `(bm-fringe-face ((,class (:background ,yellow-lc :foreground ,solarized-bg))))
     `(bm-fringe-persistent-face ((,class (:background ,green-lc :foreground ,solarized-bg))))
     `(bm-persistent-face ((,class (:background ,green-lc :foreground ,solarized-bg))))

     ;; calfw
     `(cfw:face-day-title ((,class (:background ,solarized-hl))))
     `(cfw:face-annotation ((,class (:inherit cfw:face-day-title :foreground ,yellow))))
     `(cfw:face-default-content ((,class (:foreground ,green))))
     `(cfw:face-default-day ((,class (:inherit cfw:face-day-title :weight bold))))
     `(cfw:face-disable ((,class (:inherit cfw:face-day-title :foreground ,solarized-comments))))
     `(cfw:face-grid ((,class (:foreground ,solarized-comments))))
     `(cfw:face-header ((,class (:foreground ,blue-hc :background ,blue-lc :weight bold))))
     `(cfw:face-holiday ((,class (:background nil :foreground ,red :weight bold))))
     `(cfw:face-periods ((,class (:foreground ,magenta))))
     `(cfw:face-select ((,class (:background ,magenta-lc :foreground ,magenta-hc))))
     `(cfw:face-saturday ((,class (:foreground ,cyan-hc :background ,cyan-lc))))
     `(cfw:face-sunday ((,class (:foreground ,red-hc :background ,red-lc :weight bold))))
     `(cfw:face-title ((,class (:inherit variable-pitch :foreground ,yellow :weight bold :height 2.0))))
     `(cfw:face-today ((,class (:weight bold :background ,solarized-hl :foreground nil))))
     `(cfw:face-today-title ((,class (:background ,yellow-lc :foreground ,yellow-hc :weight bold))))
     `(cfw:face-toolbar ((,class (:background ,solarized-hl :foreground ,solarized-fg))))
     `(cfw:face-toolbar-button-off ((,class (:background ,yellow-lc :foreground ,yellow-hc :weight bold))))
     `(cfw:face-toolbar-button-on ((,class (:background ,yellow-hc :foreground ,yellow-lc :weight bold))))

     ;; clojure-test-mode
     `(clojure-test-failure-face ((t (:foreground ,orange :weight bold :underline t))))
     `(clojure-test-error-face ((t (:foreground ,red :weight bold :underline t))))
     `(clojure-test-success-face ((t (:foreground ,green :weight bold :underline t))))

     ;; ctable
     `(ctbl:face-cell-select ((,class (:background ,blue :foreground ,solarized-bg))))
     `(ctbl:face-continue-bar ((,class (:background ,solarized-hl :foreground ,solarized-bg))))
     `(ctbl:face-row-select ((,class (:background ,cyan :foreground ,solarized-bg))))

     ;; coffee
     `(coffee-mode-class-name ((,class (:foreground ,yellow :weight bold))))
     `(coffee-mode-function-param ((,class (:foreground ,violet :slant italic))))

     ;; custom
     `(custom-variable-tag ((,class (:foreground ,cyan))))
     `(custom-comment-tag ((,class (:foreground ,solarized-comments))))
     `(custom-group-tag ((,class (:foreground ,blue))))
     `(custom-state ((,class (:foreground ,green))))

     ;; diff
     `(diff-added ((,class (:foreground ,green :background ,solarized-bg))))
     `(diff-changed ((,class (:foreground ,yellow :background ,solarized-bg))))
     `(diff-removed ((,class (:foreground ,red :background ,solarized-bg))))
     `(diff-header ((,class (:background ,solarized-bg))))
     `(diff-file-header
       ((,class (:background ,solarized-bg :foreground ,solarized-fg :weight bold))))

     ;; ediff
     `(ediff-fine-diff-A ((,class (:background ,orange-lc))))
     `(ediff-fine-diff-B ((,class (:background ,green-lc))))
     `(ediff-even-diff-A ((,class (:background ,solarized-comments :foreground ,solarized-fg-lc ))))
     `(ediff-odd-diff-A ((,class (:background ,solarized-comments :foreground ,solarized-fg-hc ))))
     `(ediff-even-diff-B ((,class (:background ,solarized-comments :foreground ,solarized-fg-hc ))))
     `(ediff-odd-diff-B ((,class (:background ,solarized-comments :foreground ,solarized-fg-lc ))))

     ;; epc
     `(epc:face-title ((,class (:foreground ,magenta :weight bold))))

     ;; eshell
     `(eshell-prompt ((,class (:foreground ,yellow :weight bold))))
     `(eshell-ls-archive ((,class (:foreground ,red :weight bold))))
     `(eshell-ls-backup ((,class (:inherit font-lock-comment))))
     `(eshell-ls-clutter ((,class (:inherit font-lock-comment))))
     `(eshell-ls-directory ((,class (:foreground ,blue :weight bold))))
     `(eshell-ls-executable ((,class (:foreground ,red :weight bold))))
     `(eshell-ls-unreadable ((,class (:foreground ,solarized-fg))))
     `(eshell-ls-missing ((,class (:inherit font-lock-warning))))
     `(eshell-ls-product ((,class (:inherit font-lock-doc))))
     `(eshell-ls-special ((,class (:foreground ,yellow :weight bold))))
     `(eshell-ls-symlink ((,class (:foreground ,cyan :weight bold))))

     ;; fic
     `(fic-author-face ((,class (:background ,solarized-bg :foreground ,orange :underline t :slant italic))))
     `(fic-face ((,class (:background ,solarized-bg :foreground ,orange :weight normal :slant italic))))

     ;; flymake
     `(flymake-errline
       ((,class (:foreground ,red-hc :background ,red-lc :weight bold :underline t))))
     `(flymake-infoline ((,class (:foreground ,green-hc :background ,green-lc))))
     `(flymake-warnline
       ((,class (:foreground ,yellow-hc :background ,yellow-lc :weight bold :underline t))))

     ;; flycheck
     `(flycheck-error-face
       ((,class (:foreground ,red-hc :background ,red-lc :weight bold :underline t))))
     `(flycheck-warning-face
       ((,class (:foreground ,yellow-hc :background ,yellow-lc :weight bold :underline t))))

     ;; flyspell
     `(flyspell-duplicate ((,class (:foreground ,yellow :weight bold :underline t))))
     `(flyspell-incorrect ((,class (:foreground ,red :weight bold :underline t))))

     ;; erc
     `(erc-action-face ((,class (:inherit erc-default-face))))
     `(erc-bold-face ((,class (:weight bold))))
     `(erc-current-nick-face ((,class (:foreground ,blue :weight bold))))
     `(erc-dangerous-host-face ((,class (:inherit font-lock-warning))))
     `(erc-default-face ((,class (:foreground ,solarized-fg))))
     `(erc-direct-msg-face ((,class (:inherit erc-default))))
     `(erc-error-face ((,class (:inherit font-lock-warning))))
     `(erc-fool-face ((,class (:inherit erc-default))))
     `(erc-highlight-face ((,class (:inherit hover-highlight))))
     `(erc-input-face ((,class (:foreground ,yellow))))
     `(erc-keyword-face ((,class (:foreground ,blue :weight bold))))
     `(erc-nick-default-face ((,class (:foreground ,yellow :weight bold))))
     `(erc-my-nick-face ((,class (:foreground ,red :weight bold))))
     `(erc-nick-msg-face ((,class (:inherit erc-default))))
     `(erc-notice-face ((,class (:foreground ,green))))
     `(erc-pal-face ((,class (:foreground ,orange :weight bold))))
     `(erc-prompt-face ((,class (:foreground ,orange :background ,solarized-bg :weight bold))))
     `(erc-timestamp-face ((,class (:foreground ,green))))
     `(erc-underline-face ((t (:underline t))))

     ;; git-gutter
     `(git-gutter:added ((,class (:background ,green :foreground ,solarized-bg  :weight bold))))
     `(git-gutter:deleted ((,class (:background ,red :foreground ,solarized-bg  :weight bold))))
     `(git-gutter:modified ((,class (:background ,blue :foreground ,solarized-bg :weight bold))))
     `(git-gutter:unchanged ((,class (:background ,solarized-hl :foreground ,solarized-bg  :weight bold))))
     ;; I use the following git-gutter settings along with those faces
     ;; (when window-system
     ;;  (let ((symbol "."))
     ;;    (setq git-gutter:added-sign symbol
     ;;          git-gutter:deleted-sign symbol
     ;;          git-gutter:modified-sign symbol
     ;;          git-gutter:unchanged-sign " ")))

     ;; git-gutter-fr
     `(git-gutter-fr:added ((,class (:foreground ,green  :weight bold))))
     `(git-gutter-fr:deleted ((,class (:foreground ,red :weight bold))))
     `(git-gutter-fr:modified ((,class (:foreground ,blue :weight bold))))

     ;; guide-key
     `(guide-key/highlight-command-face ((,class (:foreground ,blue))))
     `(guide-key/key-face ((,class (:foreground ,solarized-comments))))
     `(guide-key/prefix-command-face ((,class (:foreground ,green))))

     ;; gnus
     `(gnus-group-mail-1-face ((,class (:weight bold :inherit gnus-group-mail-1-empty))))
     `(gnus-group-mail-1-empty-face ((,class (:inherit gnus-group-news-1-empty))))
     `(gnus-group-mail-2-face ((,class (:weight bold :inherit gnus-group-mail-2-empty))))
     `(gnus-group-mail-2-empty-face ((,class (:inherit gnus-group-news-2-empty))))
     `(gnus-group-mail-3-face ((,class (:weight bold :inherit gnus-group-mail-3-empty))))
     `(gnus-group-mail-3-empty-face ((,class (:inherit gnus-group-news-3-empty))))
     `(gnus-group-mail-4-face ((,class (:weight bold :inherit gnus-group-mail-4-empty))))
     `(gnus-group-mail-4-empty-face ((,class (:inherit gnus-group-news-4-empty))))
     `(gnus-group-mail-5-face ((,class (:weight bold :inherit gnus-group-mail-5-empty))))
     `(gnus-group-mail-5-empty-face ((,class (:inherit gnus-group-news-5-empty))))
     `(gnus-group-mail-6-face ((,class (:weight bold :inherit gnus-group-mail-6-empty))))
     `(gnus-group-mail-6-empty-face ((,class (:inherit gnus-group-news-6-empty))))
     `(gnus-group-mail-low-face ((,class (:weight bold :inherit gnus-group-mail-low-empty))))
     `(gnus-group-mail-low-empty-face ((,class (:inherit gnus-group-news-low-empty))))
     `(gnus-group-news-1-face ((,class (:weight bold :inherit gnus-group-news-1-empty))))
     `(gnus-group-news-2-face ((,class (:weight bold :inherit gnus-group-news-2-empty))))
     `(gnus-group-news-3-face ((,class (:weight bold :inherit gnus-group-news-3-empty))))
     `(gnus-group-news-4-face ((,class (:weight bold :inherit gnus-group-news-4-empty))))
     `(gnus-group-news-5-face ((,class (:weight bold :inherit gnus-group-news-5-empty))))
     `(gnus-group-news-6-face ((,class (:weight bold :inherit gnus-group-news-6-empty))))
     `(gnus-group-news-low-face ((,class (:weight bold :inherit gnus-group-news-low-empty))))
     `(gnus-header-content-face ((,class (:inherit message-header-other))))
     `(gnus-header-from-face ((,class (:inherit message-header-from))))
     `(gnus-header-name-face ((,class (:inherit message-header-name))))
     `(gnus-header-newsgroups-face ((,class (:inherit message-header-other))))
     `(gnus-header-subject-face ((,class (:inherit message-header-subject))))
     `(gnus-summary-cancelled-face ((,class (:foreground ,orange))))
     `(gnus-summary-high-ancient-face ((,class (:foreground ,blue))))
     `(gnus-summary-high-read-face ((,class (:foreground ,green :weight bold))))
     `(gnus-summary-high-ticked-face ((,class (:foreground ,orange :weight bold))))
     `(gnus-summary-high-unread-face ((,class (:foreground ,solarized-fg :weight bold))))
     `(gnus-summary-low-ancient-face ((,class (:foreground ,blue))))
     `(gnus-summary-low-read-face ((t (:foreground ,green))))
     `(gnus-summary-low-ticked-face ((,class (:foreground ,orange :weight bold))))
     `(gnus-summary-low-unread-face ((,class (:foreground ,solarized-fg))))
     `(gnus-summary-normal-ancient-face ((,class (:foreground ,blue))))
     `(gnus-summary-normal-read-face ((,class (:foreground ,green))))
     `(gnus-summary-normal-ticked-face ((,class (:foreground ,orange :weight bold))))
     `(gnus-summary-normal-unread-face ((,class (:foreground ,solarized-fg))))
     `(gnus-summary-selected-face ((,class (:foreground ,yellow :weight bold))))
     `(gnus-cite-1-face ((,class (:foreground ,blue))))
     `(gnus-cite-10-face ((,class (:foreground ,yellow))))
     `(gnus-cite-11-face ((,class (:foreground ,yellow))))
     `(gnus-cite-2-face ((,class (:foreground ,blue))))
     `(gnus-cite-3-face ((,class (:foreground ,blue))))
     `(gnus-cite-4-face ((,class (:foreground ,green))))
     `(gnus-cite-5-face ((,class (:foreground ,green))))
     `(gnus-cite-6-face ((,class (:foreground ,green))))
     `(gnus-cite-7-face ((,class (:foreground ,red))))
     `(gnus-cite-8-face ((,class (:foreground ,red))))
     `(gnus-cite-9-face ((,class (:foreground ,red))))
     `(gnus-group-news-1-empty-face ((,class (:foreground ,yellow))))
     `(gnus-group-news-2-empty-face ((,class (:foreground ,green))))
     `(gnus-group-news-3-empty-face ((,class (:foreground ,green))))
     `(gnus-group-news-4-empty-face ((,class (:foreground ,blue))))
     `(gnus-group-news-5-empty-face ((,class (:foreground ,blue))))
     `(gnus-group-news-6-empty-face ((,class (:foreground ,solarized-bg))))
     `(gnus-group-news-low-empty-face ((,class (:foreground ,solarized-bg))))
     `(gnus-signature-face ((,class (:foreground ,yellow))))
     `(gnus-x-face ((,class (:background ,solarized-fg :foreground ,solarized-bg))))

     ;; helm (these probably needs tweaking)
     `(helm-apt-deinstalled ((,class (:foreground ,solarized-comments))))
     `(helm-apt-installed ((,class (:foreground ,green))))
     `(helm-bookmark-directory ((,class (:inherit helm-ff-directory))))
     `(helm-bookmark-file ((,class (:foreground ,solarized-fg))))
     `(helm-bookmark-gnus ((,class (:foreground ,cyan))))
     `(helm-bookmark-info ((,class (:foreground ,green))))
     `(helm-bookmark-man ((,class (:foreground ,violet))))
     `(helm-bookmark-w3m ((,class (:foreground ,yellow))))
     `(helm-bookmarks-su ((,class (:foreground ,orange))))
     `(helm-buffer-not-saved ((,class (:foreground ,orange))))
     `(helm-buffer-saved-out ((,class (:foreground ,red :background ,solarized-bg
                                                   :inverse-video t))))
     `(helm-buffer-size ((,class (:foreground ,solarized-comments))))
     `(helm-candidate-number ((,class (:background ,solarized-hl :foreground ,solarized-emph
                                                   :bold t))))
     `(helm-ff-directory ((,class (:background ,solarized-bg  :foreground ,blue))))
     `(helm-ff-executable ((,class (:foreground ,green))))
     `(helm-ff-file ((,class (:background ,solarized-bg :foreground ,solarized-fg))))
     `(helm-ff-invalid-symlink ((,class (:background ,solarized-bg :foreground ,orange
                                                     :slant italic))))
     `(helm-ff-prefix ((,class (:background ,yellow :foreground ,solarized-bg))))
     `(helm-ff-symlink ((,class (:foreground ,cyan))))
     `(helm-grep-file ((,class (:foreground ,cyan :underline t))))
     `(helm-grep-finish ((,class (:foreground ,green))))
     `(helm-grep-lineno ((,class (:foreground ,orange))))
     `(helm-grep-match ((,class (:inherit match))))
     `(helm-grep-running ((,class (:foreground ,red))))
     `(helm-header ((,class (:inherit header-line))))
     `(helm-lisp-completion-info ((,class (:foreground ,solarized-fg))))
     `(helm-lisp-show-completion ((,class (:foreground ,yellow  :background ,solarized-hl
                                                       :bold t))))
     `(helm-M-x-key ((,class (:foreground ,orange :underline t))))
     `(helm-moccur-buffer ((,class (:foreground ,cyan :underline t))))
     `(helm-match ((,class (:inherit match))))
     `(helm-selection ((,class (:background ,solarized-hl :underline t))))
     `(helm-selection-line ((,class (:background ,solarized-hl :foreground ,solarized-emph
                                                 :underline nil))))
     `(helm-separator ((,class (:foreground ,red))))
     `(helm-source-header ((,class (:background ,blue-lc :foreground ,solarized-bg
                                                :underline nil))))
     `(helm-time-zone-current ((,class (:foreground ,green))))
     `(helm-time-zone-home ((,class (:foreground ,red))))
     `(helm-visible-mark ((,class (:background ,solarized-bg :foreground ,magenta :bold t))))

     ;; hi-lock-mode
     `(hi-yellow ((,class (:foreground ,yellow-lc :background ,yellow-hc))))
     `(hi-pink ((,class (:foreground ,magenta-lc :background ,magenta-hc))))
     `(hi-green ((,class (:foreground ,green-lc :background ,green-hc))))
     `(hi-blue ((,class (:foreground ,blue-lc :background ,blue-hc))))
     `(hi-black-b ((,class (:foreground ,solarized-emph :background ,solarized-bg :weight bold))))
     `(hi-blue-b ((,class (:foreground ,blue-lc :weight bold))))
     `(hi-green-b ((,class (:foreground ,green-lc :weight bold))))
     `(hi-red-b ((,class (:foreground ,red :weight bold))))
     `(hi-black-hb ((,class (:foreground ,solarized-emph :background ,solarized-bg :weight bold))))

     ;; highlight-changes
     `(highlight-changes ((,class (:foreground ,orange))))
     `(highlight-changes-delete ((,class (:foreground ,red :underline t))))

     ;; hl-line-mode
     `(hl-line ((,class (:background ,solarized-hl))))
     `(hl-line-face ((,class (:background ,solarized-hl))))

     ;; ido-mode
     `(ido-first-match ((,class (:foreground ,green :weight bold))))
     `(ido-only-match ((,class (:foreground ,solarized-bg :background ,green :weight bold))))
     `(ido-subdir ((,class (:foreground ,blue))))
     `(ido-incomplete-regexp ((,class (:foreground ,red :weight bold ))))
     `(ido-indicator ((,class (:background ,red :foreground ,solarized-bg :width condensed))))
     `(ido-virtual ((,class (:foreground ,cyan))))

     ;; js2-mode colors
     `(js2-error ((,class (:foreground ,red))))
     `(js2-external-variable ((,class (:foreground ,orange))))
     `(js2-function-param ((,class (:foreground ,green))))
     `(js2-instance-member ((,class (:foreground ,magenta))))
     `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,cyan))))
     `(js2-jsdoc-html-tag-name ((,class (:foreground ,orange))))
     `(js2-jsdoc-tag ((,class (:foreground ,cyan))))
     `(js2-jsdoc-type ((,class (:foreground ,blue))))
     `(js2-jsdoc-value ((,class (:foreground ,violet))))
     `(js2-magic-paren ((,class (:underline t))))
     `(js2-private-function-call ((,class (:foreground ,yellow))))
     `(js2-private-member ((,class (:foreground ,blue))))
     `(js2-warning ((,class (:underline ,orange))))

     ;; jedi
     `(jedi:highlight-function-argument ((,class (:inherit bold))))

     ;; linum-mode
     `(linum ((,class (:foreground ,solarized-fg :background ,solarized-bg))))

     ;; magit
     `(magit-section-title ((,class (:foreground ,yellow :weight bold))))
     `(magit-branch ((,class (:foreground ,orange :weight bold))))
     `(magit-item-highlight ((,class (:background ,solarized-hl))))
     `(magit-log-author ((,class (:foreground ,cyan))))
     `(magit-log-graph ((,class (:foreground ,solarized-comments))))
     `(magit-log-head-label-bisect-bad ((,class (:background ,red-hc :foreground ,red-lc :box 1))))
     `(magit-log-head-label-bisect-good ((,class (:background ,green-hc :foreground ,green-lc
                                                              :box 1))))
     `(magit-log-head-label-default ((,class (:background ,solarized-hl :box 1))))
     `(magit-log-head-label-local ((,class (:background ,blue-lc :foreground ,blue-hc :box 1))))
     `(magit-log-head-label-patches ((,class (:background ,red-lc :foreground ,red-hc :box 1))))
     `(magit-log-head-label-remote ((,class (:background ,green-lc :foreground ,green-hc :box 1))))
     `(magit-log-head-label-tags ((,class (:background ,yellow-lc :foreground ,yellow-hc :box 1))))
     `(magit-log-sha1 ((,class (:foreground ,yellow))))

     ;; message-mode
     `(message-cited-text ((,class (:foreground ,solarized-comments))))
     `(message-header-name ((,class (:foreground ,green))))
     `(message-header-other ((,class (:foreground ,green))))
     `(message-header-to ((,class (:foreground ,yellow :weight bold))))
     `(message-header-cc ((,class (:foreground ,orange :weight bold))))
     `(message-header-newsgroups ((,class (:foreground ,yellow :weight bold))))
     `(message-header-subject ((,class (:foreground ,orange))))
     `(message-header-xheader ((,class (:foreground ,cyan))))
     `(message-mml ((,class (:foreground ,yellow :weight bold))))
     `(message-separator ((,class (:foreground ,solarized-comments :slant italic))))

     ;; mew
     `(mew-face-header-subject ((,class (:foreground ,orange))))
     `(mew-face-header-from ((,class (:foreground ,yellow))))
     `(mew-face-header-date ((,class (:foreground ,green))))
     `(mew-face-header-to ((,class (:foreground ,red))))
     `(mew-face-header-key ((,class (:foreground ,green))))
     `(mew-face-header-private ((,class (:foreground ,green))))
     `(mew-face-header-important ((,class (:foreground ,blue))))
     `(mew-face-header-marginal ((,class (:foreground ,solarized-fg :weight bold))))
     `(mew-face-header-warning ((,class (:foreground ,red))))
     `(mew-face-header-xmew ((,class (:foreground ,green))))
     `(mew-face-header-xmew-bad ((,class (:foreground ,red))))
     `(mew-face-body-url ((,class (:foreground ,orange))))
     `(mew-face-body-comment ((,class (:foreground ,solarized-fg :slant italic))))
     `(mew-face-body-cite1 ((,class (:foreground ,green))))
     `(mew-face-body-cite2 ((,class (:foreground ,blue))))
     `(mew-face-body-cite3 ((,class (:foreground ,orange))))
     `(mew-face-body-cite4 ((,class (:foreground ,yellow))))
     `(mew-face-body-cite5 ((,class (:foreground ,red))))
     `(mew-face-mark-review ((,class (:foreground ,blue))))
     `(mew-face-mark-escape ((,class (:foreground ,green))))
     `(mew-face-mark-delete ((,class (:foreground ,red))))
     `(mew-face-mark-unlink ((,class (:foreground ,yellow))))
     `(mew-face-mark-refile ((,class (:foreground ,green))))
     `(mew-face-mark-unread ((,class (:foreground ,red))))
     `(mew-face-eof-message ((,class (:foreground ,green))))
     `(mew-face-eof-part ((,class (:foreground ,yellow))))

     ;; mingus
     `(mingus-directory-face ((,class (:foreground ,blue))))
     `(mingus-pausing-face ((,class (:foreground ,magenta))))
     `(mingus-playing-face ((,class (:foreground ,cyan))))
     `(mingus-playlist-face ((,class (:foreground ,cyan ))))
     `(mingus-song-file-face ((,class (:foreground ,yellow))))
     `(mingus-stopped-face ((,class (:foreground ,red))))

     ;; moccur
     `(moccur-current-line-face ((,class (:underline t))))
     `(moccur-edit-done-face ((,class
                               (:foreground ,solarized-comments
                                            :background ,solarized-bg
                                            :slant italic))))
     `(moccur-edit-face
       ((,class (:background ,yellow :foreground ,solarized-bg))))
     `(moccur-edit-file-face ((,class (:background ,solarized-hl))))
     `(moccur-edit-reject-face ((,class (:foreground ,red))))
     `(moccur-face ((,class (:background ,solarized-hl :foreground ,solarized-emph
                                         :weight bold))))
     `(search-buffers-face ((,class (:background ,solarized-hl :foreground ,solarized-emph
                                                 :weight bold))))
     `(search-buffers-header-face ((,class (:background ,solarized-hl :foreground ,yellow
                                                        :weight bold))))

     ;; mu4e
     `(mu4e-cited-1-face ((,class (:foreground ,green :slant italic :weight normal))))
     `(mu4e-cited-2-face ((,class (:foreground ,blue :slant italic :weight normal))))
     `(mu4e-cited-3-face ((,class (:foreground ,orange :slant italic :weight normal))))
     `(mu4e-cited-4-face ((,class (:foreground ,yellow :slant italic :weight normal))))
     `(mu4e-cited-5-face ((,class (:foreground ,cyan :slant italic :weight normal))))
     `(mu4e-cited-6-face ((,class (:foreground ,green :slant italic :weight normal))))
     `(mu4e-cited-7-face ((,class (:foreground ,blue :slant italic :weight normal))))
     `(mu4e-flagged-face ((,class (:foreground ,magenta :weight bold))))
     `(mu4e-view-url-number-face ((,class (:foreground ,orange :weight bold))))
     `(mu4e-warning-face ((,class (:foreground ,red :slant normal :weight bold))))

     ;; mumamo
     `(mumamo-background-chunk-submode1 ((,class (:background ,solarized-hl))))

     ;; nav
     `(nav-face-heading ((,class (:foreground ,yellow))))
     `(nav-face-button-num ((,class (:foreground ,cyan))))
     `(nav-face-dir ((,class (:foreground ,green))))
     `(nav-face-hdir ((,class (:foreground ,red))))
     `(nav-face-file ((,class (:foreground ,solarized-fg))))
     `(nav-face-hfile ((,class (:foreground ,red))))

     ;; nav-flash
     `(nav-flash-face ((,class (:background ,solarized-hl))))

     ;; org-mode
     `(org-agenda-structure
       ((,class (:inherit font-lock-comment-face :foreground ,magenta :inverse-video t))))
     `(org-agenda-date
       ((,class (:foreground ,solarized-fg :background ,solarized-hl :weight bold
                             :box (:line-width 4 :color ,solarized-hl) ))) t)
     `(org-agenda-date-weekend ((,class (:inherit org-agenda-date :slant italic))) t)
     `(org-agenda-date-today
       ((,class (:inherit org-agenda-date :slant italic underline: t))) t)
     `(org-agenda-done ((,class (:foreground ,green))) t)
     `(org-archived ((,class (:foreground ,solarized-comments :weight normal))))
     `(org-block ((,class (:foreground ,solarized-comments))))
     `(org-block-begin-line ((,class (:foreground ,solarized-comments :slant italic))))
     `(org-checkbox ((,class (:background ,solarized-bg :foreground ,solarized-fg
                                          :box (:line-width 1 :style released-button)))))
     `(org-code ((,class (:foreground ,solarized-comments))))
     `(org-date ((,class (:foreground ,blue :underline t))))
     `(org-done ((,class (:weight bold :foreground ,green))))
     `(org-ellipsis ((,class (:foreground ,solarized-comments))))
     `(org-formula ((,class (:foreground ,yellow))))
     `(org-headline-done ((,class (:foreground ,green))))
     `(org-hide ((,class (:foreground ,solarized-bg))))
     `(org-level-1 ((,class (:foreground ,orange))))
     `(org-level-2 ((,class (:foreground ,green))))
     `(org-level-3 ((,class (:foreground ,blue))))
     `(org-level-4 ((,class (:foreground ,yellow))))
     `(org-level-5 ((,class (:foreground ,cyan))))
     `(org-level-6 ((,class (:foreground ,green))))
     `(org-level-7 ((,class (:foreground ,red))))
     `(org-level-8 ((,class (:foreground ,blue))))
     `(org-link ((,class (:foreground ,yellow :underline t))))
     `(org-sexp-date ((,class (:foreground ,violet))))
     `(org-scheduled ((,class (:foreground ,green))))
     `(org-scheduled-previously ((,class (:foreground ,yellow))))
     `(org-scheduled-today ((,class (:foreground ,blue :weight normal))))
     `(org-special-keyword ((,class (:foreground ,solarized-comments :weight bold))))
     `(org-table ((,class (:foreground ,green))))
     `(org-tag ((,class (:weight bold))))
     `(org-time-grid ((,class (:foreground ,cyan))))
     `(org-todo ((,class (:foreground ,red :weight bold))))
     `(org-upcoming-deadline ((,class (:foreground ,yellow  :weight normal :underline nil))))
     `(org-warning ((,class (:foreground ,orange :weight normal :underline nil))))
     ;; org-habit (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
     `(org-habit-clear-face ((,class (:background ,blue-lc :foreground ,blue-hc))))
     `(org-habit-clear-future-face ((,class (:background ,blue-lc))))
     `(org-habit-ready-face ((,class (:background ,green-lc :foreground ,green))))
     `(org-habit-ready-future-face ((,class (:background ,green-lc))))
     `(org-habit-alert-face ((,class (:background ,yellow :foreground ,yellow-lc))))
     `(org-habit-alert-future-face ((,class (:background ,yellow-lc))))
     `(org-habit-overdue-face ((,class (:background ,red :foreground ,red-lc))))
     `(org-habit-overdue-future-face ((,class (:background ,red-lc))))
     ;; latest additions
     `(org-agenda-dimmed-todo-face ((,class (:foreground ,solarized-comments))))
     `(org-agenda-restriction-lock ((,class (:background ,yellow))))
     `(org-clock-overlay ((,class (:background ,yellow))))
     `(org-column ((,class (:background ,solarized-hl :strike-through nil
                                        :underline nil :slant normal :weight normal))))
     `(org-column-title ((,class (:background ,solarized-hl :underline t :weight bold))))
     `(org-date-selected ((,class (:foreground ,red :inverse-video t))))
     `(org-document-info ((,class (:foreground ,solarized-fg))))
     `(org-document-title ((,class (:foreground ,solarized-emph  :weight bold :height 1.44))))
     `(org-drawer ((,class (:foreground ,cyan))))
     `(org-footnote ((,class (:foreground ,magenta :underline t))))
     `(org-latex-and-export-specials ((,class (:foreground ,orange))))
     `(org-mode-line-clock-overrun ((,class (:inherit modeline :background ,red))))

     ;; outline
     `(outline-8 ((,class (:inherit default))))
     `(outline-7 ((,class (:inherit outline-8 :height 1.0))))
     `(outline-6 ((,class (:inherit outline-7 :height 1.0))))
     `(outline-5 ((,class (:inherit outline-6 :height 1.0))))
     `(outline-4 ((,class (:inherit outline-5 :height 1.0))))
     `(outline-3 ((,class (:inherit outline-4 :height 1.0))))
     `(outline-2 ((,class (:inherit outline-3 :height 1.0))))
     `(outline-1 ((,class (:inherit outline-2 :height 1.0))))

     ;; pretty-mode
     `(pretty-mode-symbol-face  ((,class (:foreground ,green))))

     ;; popup
     `(popup-face ((,class (:background ,solarized-hl :foreground ,solarized-fg))))
     `(popup-isearch-match ((,class (:background ,yellow :foreground ,solarized-bg))))
     `(popup-menu-face ((,class (:background ,solarized-hl :foreground ,solarized-fg))))
     `(popup-menu-mouse-face ((,class (:background ,blue :foreground ,solarized-fg))))
     `(popup-menu-selection-face ((,class (:background ,magenta :foreground ,solarized-bg))))
     `(popup-scroll-bar-background-face ((,class (:background ,solarized-comments))))
     `(popup-scroll-bar-foreground-face ((,class (:background ,solarized-emph))))
     `(popup-tip-face ((,class (:background ,solarized-hl :foreground ,solarized-fg))))

     ;; rainbow-delimiters
     `(rainbow-delimiters-depth-1-face ((,class (:foreground ,cyan))))
     `(rainbow-delimiters-depth-2-face ((,class (:foreground ,yellow))))
     `(rainbow-delimiters-depth-3-face ((,class (:foreground ,blue))))
     `(rainbow-delimiters-depth-4-face ((,class (:foreground ,orange))))
     `(rainbow-delimiters-depth-5-face ((,class (:foreground ,green))))
     `(rainbow-delimiters-depth-6-face ((,class (:foreground ,yellow))))
     `(rainbow-delimiters-depth-7-face ((,class (:foreground ,blue))))
     `(rainbow-delimiters-depth-8-face ((,class (:foreground ,orange))))
     `(rainbow-delimiters-depth-9-face ((,class (:foreground ,green))))
     `(rainbow-delimiters-depth-10-face ((,class (:foreground ,yellow))))
     `(rainbow-delimiters-depth-11-face ((,class (:foreground ,blue))))
     `(rainbow-delimiters-depth-12-face ((,class (:foreground ,orange))))
     `(rainbow-delimiters-unmatched-face
       ((,class (:foreground ,solarized-fg :background ,solarized-bg :inverse-video t))))

     ;; rst-mode
     `(rst-level-1-face ((,class (:background ,yellow   :foreground ,solarized-bg))))
     `(rst-level-2-face ((,class (:background ,cyan    :foreground ,solarized-bg))))
     `(rst-level-3-face ((,class (:background ,blue    :foreground ,solarized-bg))))
     `(rst-level-4-face ((,class (:background ,violet  :foreground ,solarized-bg))))
     `(rst-level-5-face ((,class (:background ,magenta :foreground ,solarized-bg))))
     `(rst-level-6-face ((,class (:background ,red     :foreground ,solarized-bg))))

     ;; rpm-mode
     `(rpm-spec-dir-face ((,class (:foreground ,green))))
     `(rpm-spec-doc-face ((,class (:foreground ,green))))
     `(rpm-spec-ghost-face ((,class (:foreground ,red))))
     `(rpm-spec-macro-face ((,class (:foreground ,yellow))))
     `(rpm-spec-obsolete-tag-face ((,class (:foreground ,red))))
     `(rpm-spec-package-face ((,class (:foreground ,red))))
     `(rpm-spec-section-face ((,class (:foreground ,yellow))))
     `(rpm-spec-tag-face ((,class (:foreground ,blue))))
     `(rpm-spec-var-face ((,class (:foreground ,red))))

     ;; sh-mode
     `(sh-quoted-exec ((,class (:foreground ,violet :weight bold))))
     `(sh-escaped-newline ((,class (:foreground ,yellow :weight bold))))
     `(sh-heredoc ((,class (:foreground ,yellow :weight bold))))

     ;; smartparens
     `(sp-pair-overlay-face ((,class (:background ,solarized-hl))))
     `(sp-wrap-overlay-face ((,class (:background ,solarized-hl))))
     `(sp-wrap-tag-overlay-face ((,class (:background ,solarized-hl))))

     ;; show-paren
     `(show-paren-match
       ((,class (:foreground ,cyan :background ,solarized-bg :weight normal :inverse-video t))))
     `(show-paren-mismatch
       ((,class (:foreground ,red :background ,solarized-bg :weight normal :inverse-video t))))

     ;; mic-paren
     `(paren-face-match
       ((,class (:foreground ,cyan :background ,solarized-bg :weight normal :inverse-video t))))
     `(paren-face-mismatch
       ((,class (:foreground ,red :background ,solarized-bg :weight normal :inverse-video t))))
     `(paren-face-no-match
       ((,class (:foreground ,red :background ,solarized-bg :weight normal :inverse-video t))))

     ;; SLIME
     `(slime-repl-inputed-output-face ((,class (:foreground ,red))))

     ;; speedbar
     `(speedbar-button-face ((,class (:inherit variable-pitch :foreground ,solarized-comments))))
     `(speedbar-directory-face ((,class (:inherit variable-pitch :foreground ,blue))))
     `(speedbar-file-face ((,class (:inherit variable-pitch :foreground ,solarized-fg))))
     `(speedbar-highlight-face ((,class (:inherit variable-pitch :background ,solarized-hl))))
     `(speedbar-selected-face ((,class (:inherit variable-pitch :foreground ,yellow :underline t))))
     `(speedbar-separator-face ((,class (:inherit variable-pitch
                                                  :background ,blue :foreground ,solarized-bg
                                                  :overline ,cyan-lc))))
     `(speedbar-tag-face ((,class (:inherit variable-pitch :foreground ,green))))

     ;; sunrise commander headings
     `(sr-active-path-face ((,class (:background ,blue :foreground ,solarized-bg
                                                 :height 100  :weight bold))))
     `(sr-editing-path-face ((,class (:background ,yellow :foreground ,solarized-bg
                                                  :weight bold :height 100))))
     `(sr-highlight-path-face ((,class (:background ,green :foreground ,solarized-bg
                                                    :weight bold :height 100))))
     `(sr-passive-path-face ((,class (:background ,solarized-comments :foreground ,solarized-bg
                                                  :weight bold :height 100))))
     ;; sunrise commander marked
     `(sr-marked-dir-face ((,class (:inherit dired-marked))))
     `(sr-marked-file-face ((,class (:inherit dired-marked))))
     `(sr-alt-marked-dir-face ((,class (:background ,magenta :foreground ,solarized-bg
                                                    :weight bold))))
     `(sr-alt-marked-file-face ((,class (:background ,magenta :foreground ,solarized-bg
                                                     :weight bold))))
     ;; sunrise commander fstat
     `(sr-directory-face ((,class (:inherit dired-directory :weight normal))))
     `(sr-symlink-directory-face ((,class (:inherit dired-directory :slant italic :weight normal))))
     `(sr-symlink-face ((,class (:inherit dired-symlink :slant italic :weight normal))))
     `(sr-broken-link-face ((,class (:inherit dired-warning :slant italic :weight normal))))
     ;; sunrise commander file types
     `(sr-compressed-face ((,class (:foreground ,solarized-fg))))
     `(sr-encrypted-face ((,class (:foreground ,solarized-fg))))
     `(sr-log-face ((,class (:foreground ,solarized-fg))))
     `(sr-packaged-face ((,class (:foreground ,solarized-fg))))
     `(sr-html-face ((,class (:foreground ,solarized-fg))))
     `(sr-xml-face ((,class (:foreground ,solarized-fg))))
     ;; sunrise commander misc
     `(sr-clex-hotchar-face ((,class (:background ,red  :foreground ,solarized-bg :weight bold))))

     ;; table
     `(table-cell ((,class (:foreground ,solarized-fg :background ,solarized-hl))))

     ;; term
     `(term-color-black ((t (:foreground ,base03
                                         :background ,base02))))
     `(term-color-red ((t (:foreground ,red
                                       :background ,red-d))))
     `(term-color-green ((t (:foreground ,green
                                         :background ,green-d))))
     `(term-color-yellow ((t (:foreground ,yellow
                                          :background ,yellow-d))))
     `(term-color-blue ((t (:foreground ,blue
                                        :background ,blue-d))))
     `(term-color-magenta ((t (:foreground ,magenta
                                           :background ,magenta-d))))
     `(term-color-cyan ((t (:foreground ,cyan
                                        :background ,cyan-d))))
     `(term-color-white ((t (:foreground ,base00
                                         :background ,base0))))
     '(term-default-fg-color ((t (:inherit term-color-white))))
     '(term-default-bg-color ((t (:inherit term-color-black))))


     ;; tooltip. (NOTE: This setting has no effect on the os widgets for me
     ;; zencoding uses this)
     `(tooltip ((,class (:background ,yellow-lc :foreground ,yellow-hc
                                     :inherit variable-pitch))))

     ;; tuareg
     `(tuareg-font-lock-governing-face ((,class (:foreground ,magenta :weight bold))))
     `(tuareg-font-lock-multistage-face ((,class (:foreground ,blue :background ,solarized-hl :weight bold))))
     `(tuareg-font-lock-operator-face ((,class (:foreground ,solarized-emph))))
     `(tuareg-font-lock-error-face ((,class (:foreground ,yellow :background ,red :weight bold))))
     `(tuareg-font-lock-interactive-output-face ((,class (:foreground ,cyan))))
     `(tuareg-font-lock-interactive-error-face ((,class (:foreground ,red))))

     ;; undo-tree
     `(undo-tree-visualizer-default-face
       ((,class (:foreground ,solarized-comments :background ,solarized-bg))))
     `(undo-tree-visualizer-unmodified-face ((,class (:foreground ,green))))
     `(undo-tree-visualizer-current-face ((,class (:foreground ,blue :inverse-video t))))
     `(undo-tree-visualizer-active-branch-face
       ((,class (:foreground ,solarized-emph :background ,solarized-bg :weight bold))))
     `(undo-tree-visualizer-register-face ((,class (:foreground ,yellow))))

     ;; volatile highlights
     `(vhl/default-face ((,class (:background ,green-lc :foreground ,green-hc))))

     ;; w3m
     `(w3m-anchor ((,class (:inherit link))))
     `(w3m-arrived-anchor ((,class (:inherit link-visited))))
     `(w3m-form ((,class (:background ,solarized-bg :foreground ,solarized-fg))))
     `(w3m-header-line-location-title ((,class (:background ,solarized-hl :foreground ,yellow))))
     `(w3m-header-line-location-content ((,class (:background ,solarized-hl :foreground ,solarized-fg))))
     `(w3m-bold ((,class (:foreground ,solarized-emph :weight bold))))
     `(w3m-image-anchor ((,class (:background ,solarized-bg :foreground ,cyan :inherit link))))
     `(w3m-image ((,class (:background ,solarized-bg :foreground ,cyan))))
     `(w3m-lnum-minibuffer-prompt ((,class (:foreground ,solarized-emph))))
     `(w3m-lnum-match ((,class (:background ,solarized-hl))))
     `(w3m-lnum ((,class (:underline nil :bold nil :foreground ,red))))
     `(w3m-session-select ((,class (:foreground ,solarized-fg))))
     `(w3m-session-selected ((,class (:foreground ,solarized-emph :bold t :underline t))))
     `(w3m-tab-background ((,class (:background ,solarized-bg :foreground ,solarized-fg))))
     `(w3m-tab-selected-background ((,class (:background ,solarized-bg :foreground ,solarized-fg))))
     `(w3m-tab-mouse ((,class (:background ,solarized-hl :foreground ,yellow))))
     `(w3m-tab-selected ((,class (:background ,solarized-hl :foreground ,solarized-emph :bold t))))
     `(w3m-tab-unselected ((,class (:background ,solarized-hl :foreground ,solarized-fg))))
     `(w3m-tab-selected-retrieving ((,class (:background ,solarized-hl :foreground ,red))))
     `(w3m-tab-unselected-retrieving ((,class (:background ,solarized-hl :foreground ,orange))))
     `(w3m-tab-unselected-unseen ((,class (:background ,solarized-hl :foreground ,violet))))

     ;; web-mode
     `(web-mode-builtin-face ((,class (:foreground ,red))))
     `(web-mode-comment-face ((,class (:foreground ,solarized-comments))))
     `(web-mode-constant-face ((,class (:foreground ,blue :weight bold))))
     `(web-mode-css-at-rule-face ((,class (:foreground ,violet :slant italic))))
     `(web-mode-css-prop-face ((,class (:foreground ,violet))))
     `(web-mode-css-pseudo-class-face ((,class (:foreground ,green :slant italic))))
     `(web-mode-css-rule-face ((,class (:foreground ,blue))))
     `(web-mode-doctype-face ((,class (:foreground ,solarized-comments
                                                   :slant italic :weight bold))))
     `(web-mode-folded-face ((,class (:underline t))))
     `(web-mode-function-name-face ((,class (:foreground ,blue))))
     `(web-mode-html-attr-name-face ((,class (:foreground ,blue :slant normal))))
     `(web-mode-html-attr-value-face ((,class (:foreground ,cyan :slant italic))))
     `(web-mode-html-tag-face ((,class (:foreground ,green))))
     `(web-mode-keyword-face ((,class (:foreground ,yellow :weight bold))))
     `(web-mode-preprocessor-face ((,class (:foreground ,yellow  :slant italic :weight bold))))
     `(web-mode-string-face ((,class (:foreground ,cyan))))
     `(web-mode-type-face ((,class (:foreground ,yellow))))
     `(web-mode-variable-name-face ((,class (:foreground ,blue))))

     ;; whitespace-mode
     `(whitespace-space ((,class (:background ,solarized-bg :foreground ,yellow-lc
                                              :inverse-video t))))
     `(whitespace-hspace ((,class (:background ,solarized-bg :foreground ,red-lc
                                               :inverse-video t))))
     `(whitespace-tab ((,class (:background ,solarized-bg :foreground ,orange-lc
                                            :inverse-video t))))
     `(whitespace-newline ((,class (:foreground ,solarized-comments))))
     `(whitespace-trailing ((,class (:foreground ,blue-lc :background ,solarized-bg
                                                 :inverse-video t))))
                                        ; removing inverse video on this
     `(whitespace-line ((,class (:background ,solarized-bg :foreground ,magenta
                                             :inverse-video nil))))
     `(whitespace-space-before-tab ((,class (:background ,solarized-bg :foreground ,green-lc
                                                         :inverse-video t))))
     `(whitespace-indentation ((,class (:background ,solarized-bg :foreground ,magenta-lc
                                                    :inverse-video t))))
     `(whitespace-empty ((,class (:background ,solarized-fg :foreground ,red-lc :inverse-video t))))
     `(whitespace-space-after-tab ((,class (:background ,solarized-bg  :foreground ,violet-lc
                                                        :inverse-video t))))

     ;; wanderlust
     `(wl-highlight-folder-few-face ((,class (:foreground ,red))))
     `(wl-highlight-folder-many-face ((,class (:foreground ,red))))
     `(wl-highlight-folder-path-face ((,class (:foreground ,orange))))
     `(wl-highlight-folder-unread-face ((,class (:foreground ,blue))))
     `(wl-highlight-folder-zero-face ((,class (:foreground ,solarized-fg))))
     `(wl-highlight-folder-unknown-face ((,class (:foreground ,blue))))
     `(wl-highlight-message-citation-header ((,class (:foreground ,red))))
     `(wl-highlight-message-cited-text-1 ((,class (:foreground ,red))))
     `(wl-highlight-message-cited-text-2 ((,class (:foreground ,green))))
     `(wl-highlight-message-cited-text-3 ((,class (:foreground ,blue))))
     `(wl-highlight-message-cited-text-4 ((,class (:foreground ,blue))))
     `(wl-highlight-message-header-contents-face ((,class (:foreground ,green))))
     `(wl-highlight-message-headers-face ((,class (:foreground ,red))))
     `(wl-highlight-message-important-header-contents ((,class (:foreground ,green))))
     `(wl-highlight-message-header-contents ((,class (:foreground ,green))))
     `(wl-highlight-message-important-header-contents2 ((,class (:foreground ,green))))
     `(wl-highlight-message-signature ((,class (:foreground ,green))))
     `(wl-highlight-message-unimportant-header-contents ((,class (:foreground ,solarized-fg))))
     `(wl-highlight-summary-answered-face ((,class (:foreground ,blue))))
     `(wl-highlight-summary-disposed-face ((,class (:foreground ,solarized-fg
                                                                :slant italic))))
     `(wl-highlight-summary-new-face ((,class (:foreground ,blue))))
     `(wl-highlight-summary-normal-face ((,class (:foreground ,solarized-fg))))
     `(wl-highlight-summary-thread-top-face ((,class (:foreground ,yellow))))
     `(wl-highlight-thread-indent-face ((,class (:foreground ,magenta))))
     `(wl-highlight-summary-refiled-face ((,class (:foreground ,solarized-fg))))
     `(wl-highlight-summary-displaying-face ((,class (:underline t :weight bold))))

     ;; which-func-mode
     `(which-func ((,class (:foreground ,green))))

     ;; window-number-mode
     `(window-number-face ((,class (:foreground ,green))))

     ;; yascroll
     `(yascroll:thumb-text-area
       ((,class (:foreground ,solarized-comments :background ,solarized-comments))))
     `(yascroll:thumb-fringe
       ((,class (:foreground ,solarized-comments :background ,solarized-comments))))

     ;; zencoding
     `(zencoding-preview-input ((,class (:background ,solarized-hl :box ,solarized-emph)))))


    (custom-theme-set-variables
     theme-name
     `(ansi-color-names-vector [,solarized-bg ,red ,green ,yellow
                                             ,blue ,magenta ,cyan ,solarized-fg])
     ;; fill-column-indicator
     `(fci-rule-color ,solarized-hl)

     ;; highlight-changes
     `(highlight-changes-colors '(,magenta ,violet))

     ;; highlight-tail
     `(highlight-tail-colors
       '((,solarized-hl . 0)(,green-lc . 20)(,cyan-lc . 30)(,blue-lc . 50)
         (,yellow-lc . 60)(,orange-lc . 70)(,magenta-lc . 85)(,solarized-hl . 100)))

     ;; vc-annotate
     `(vc-annotate-color-map
       '(( 20. . ,red-d)
         ( 40. . ,red-l)
         ( 60. . ,orange)
         ( 80. . ,yellow-d)
         (100. . ,yellow)
         (120. . ,yellow-l)
         (140. . ,green-d)
         (160. . ,green)
         (180. . ,green-l)
         (200. . ,violet-d)
         (220. . ,violet)
         (240. . ,violet-l)
         (260. . ,cyan)
         (280. . ,cyan-l)
         (300. . ,blue-d)
         (320. . ,blue)
         (340. . ,blue-l)
         (360. . ,magenta)))
     `(vc-annotate-very-old-color ,magenta-d)
     `(vc-annotate-background ,solarized-emph))

    ;; call chained theme function
    (when childtheme (funcall childtheme))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'solarized)

;;; solarized.el ends here
