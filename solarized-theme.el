;;; solarized-theme.el --- Solarized for Emacs.

;; Copyright (C) 2011,2012 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar.batsov@gmail.com>
;; Author: Thomas Frössman <thomasf@jossystem.se>
;; URL: http://github.com/bbatsov/solarized-emacs
;; Version: 0.5.0

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
;;; Code

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
     `(link ((,class (:foreground ,yellow :underline t :weight bold))))
     `(link-visited ((,class (:foreground ,yellow :underline t :weight normal))))

     ;; basic coloring
     `(default ((,class (:foreground ,solarized-fg :background ,solarized-bg))))
     `(cursor ((,class (:foreground ,solarized-bg :background ,solarized-fg :inverse-video t))))
     `(escape-glyph-face ((,class (:foreground ,red))))
     `(fringe ((,class (:foreground ,solarized-fg :background ,solarized-hl))))
     `(header-line ((,class (:foreground ,yellow
                                         :background ,solarized-hl
                                         :box (:line-width -1 :style released-button)))))
     `(highlight ((,class (:background ,solarized-hl))))

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

     ;; grep
     `(grep-context-face ((,class (:foreground ,solarized-fg))))
     `(grep-error-face ((,class (:foreground ,red :weight bold :underline t))))
     `(grep-hit-face ((,class (:foreground ,blue))))
     `(grep-match-face ((,class (:foreground ,orange :weight bold))))
     `(match ((,class (:background ,solarized-hl :foreground ,solarized-emph :weight bold))))

     ;; faces used by isearch
     `(isearch ((,class (:foreground ,yellow :background ,solarized-hl))))
     `(isearch-fail ((,class (:foreground ,solarized-fg :background ,red))))
     `(lazy-highlight ((,class (:foreground ,yellow :background ,solarized-hl))))

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
     `(secondary-selection ((,class (:background ,solarized-bg))))
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
     `(font-lock-warning-face ((,class (:foreground ,red :weight bold :underline t))))

     `(c-annotation-face ((,class (:inherit font-lock-constant-face))))

     ;;; external

     ;; ace-jump-mode
     `(ace-jump-face-background
       ((,class (:foreground ,solarized-comments :background ,solarized-bg :inverse-video nil))))
     `(ace-jump-face-foreground
       ((,class (:foreground ,red :background ,solarized-bg :inverse-video nil))))

     ;; auto highlight symbol
     `(ahs-definition-face ((,class (:foreground ,solarized-bg :background ,blue :underline t))))
     `(ahs-edit-mode-face ((,class (:foreground ,solarized-bg :background ,yellow))))
     `(ahs-face ((,class (:foreground ,solarized-bg :background ,blue))))
     `(ahs-plugin-bod-face ((,class (:foreground ,solarized-bg :background ,blue))))
     `(ahs-plugin-defalt-face ((,class (:foreground ,solarized-bg :background ,cyan))))
     `(ahs-plugin-whole-buffer-face ((,class (:foreground ,solarized-bg :background ,green))))
     `(ahs-warning-face ((,class (:foreground ,red :bold t))))

     ;; custom
     `(custom-variable-tag ((,class (:foreground ,cyan))))
     `(custom-comment-tag ((,class (:foreground ,solarized-comments))))
     `(custom-group-tag ((,class (:foreground ,blue))))
     `(custom-state ((,class (:foreground ,green))))

     ;; diff
     `(diff-added ((,class (:foreground ,green))))
     `(diff-changed ((,class (:foreground ,yellow))))
     `(diff-removed ((,class (:foreground ,red))))
     `(diff-header ((,class (:background ,solarized-bg))))
     `(diff-file-header
       ((,class (:background ,solarized-bg :foreground ,solarized-fg :bold t))))

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

     ;; flymake
     `(flymake-errline
       ((,class (:foreground ,red-hc :background ,red-lc  :weight bold :underline t))))
     `(flymake-infoline ((,class (:foreground ,green-hc :background ,green-lc))))
     `(flymake-warnline
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
     `(erc-my-nick-face ((,class (:foreground ,red :weigth bold))))
     `(erc-nick-msg-face ((,class (:inherit erc-default))))
     `(erc-notice-face ((,class (:foreground ,green))))
     `(erc-pal-face ((,class (:foreground ,orange :weight bold))))
     `(erc-prompt-face ((,class (:foreground ,orange :background ,solarized-bg :weight bold))))
     `(erc-timestamp-face ((,class (:foreground ,green))))
     `(erc-underline-face ((t (:underline t))))

     ;; gnus
     `(gnus-group-mail-1-face ((,class (:bold t :inherit gnus-group-mail-1-empty))))
     `(gnus-group-mail-1-empty-face ((,class (:inherit gnus-group-news-1-empty))))
     `(gnus-group-mail-2-face ((,class (:bold t :inherit gnus-group-mail-2-empty))))
     `(gnus-group-mail-2-empty-face ((,class (:inherit gnus-group-news-2-empty))))
     `(gnus-group-mail-3-face ((,class (:bold t :inherit gnus-group-mail-3-empty))))
     `(gnus-group-mail-3-empty-face ((,class (:inherit gnus-group-news-3-empty))))
     `(gnus-group-mail-4-face ((,class (:bold t :inherit gnus-group-mail-4-empty))))
     `(gnus-group-mail-4-empty-face ((,class (:inherit gnus-group-news-4-empty))))
     `(gnus-group-mail-5-face ((,class (:bold t :inherit gnus-group-mail-5-empty))))
     `(gnus-group-mail-5-empty-face ((,class (:inherit gnus-group-news-5-empty))))
     `(gnus-group-mail-6-face ((,class (:bold t :inherit gnus-group-mail-6-empty))))
     `(gnus-group-mail-6-empty-face ((,class (:inherit gnus-group-news-6-empty))))
     `(gnus-group-mail-low-face ((,class (:bold t :inherit gnus-group-mail-low-empty))))
     `(gnus-group-mail-low-empty-face ((,class (:inherit gnus-group-news-low-empty))))
     `(gnus-group-news-1-face ((,class (:bold t :inherit gnus-group-news-1-empty))))
     `(gnus-group-news-2-face ((,class (:bold t :inherit gnus-group-news-2-empty))))
     `(gnus-group-news-3-face ((,class (:bold t :inherit gnus-group-news-3-empty))))
     `(gnus-group-news-4-face ((,class (:bold t :inherit gnus-group-news-4-empty))))
     `(gnus-group-news-5-face ((,class (:bold t :inherit gnus-group-news-5-empty))))
     `(gnus-group-news-6-face ((,class (:bold t :inherit gnus-group-news-6-empty))))
     `(gnus-group-news-low-face ((,class (:bold t :inherit gnus-group-news-low-empty))))
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

     ;; hl-line-mode
     `(hl-line-face ((,class (:background ,solarized-bg))))

     ;; ido-mode
     `(ido-first-match ((,class (:foreground ,yellow :weight bold))))
     `(ido-only-match ((,class (:foreground ,orange :weight bold))))
     `(ido-subdir ((,class (:foreground ,yellow))))

     ;; linum-mode
     `(linum ((,class (:foreground ,solarized-fg :background ,solarized-bg))))

     ;; magit
     `(magit-section-title ((,class (:foreground ,yellow :weight bold))))
     `(magit-branch ((,class (:foreground ,orange :weight bold))))

     ;; message-mode
     `(message-cited-text-face ((,class (:inherit font-lock-comment))))
     `(message-header-name-face ((,class (:foreground ,green))))
     `(message-header-other-face ((,class (:foreground ,green))))
     `(message-header-to-face ((,class (:foreground ,yellow :weight bold))))
     `(message-header-from-face ((,class (:foreground ,yellow :weight bold))))
     `(message-header-cc-face ((,class (:foreground ,yellow :weight bold))))
     `(message-header-newsgroups-face ((,class (:foreground ,yellow :weight bold))))
     `(message-header-subject-face ((,class (:foreground ,orange :weight bold))))
     `(message-header-xheader-face ((,class (:foreground ,green))))
     `(message-mml-face ((,class (:foreground ,yellow :weight bold))))
     `(message-separator-face ((,class (:inherit font-lock-comment))))

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

     ;; nav
     `(nav-face-heading ((,class (:foreground ,yellow))))
     `(nav-face-button-num ((,class (:foreground ,cyan))))
     `(nav-face-dir ((,class (:foreground ,green))))
     `(nav-face-hdir ((,class (:foreground ,red))))
     `(nav-face-file ((,class (:foreground ,solarized-fg))))
     `(nav-face-hfile ((,class (:foreground ,red))))

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
     `(org-done ((,class (:bold t :weight bold :foreground ,green))))
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
     `(org-scheduled-previously ((,class (:foreground ,orange))))
     `(org-scheduled-today ((,class (:bold t :foreground ,blue :weight bold))))
     `(org-special-keyword ((,class (:foreground ,solarized-comments :weigth bold :bold t))))
     `(org-table ((,class (:foreground ,green))))
     `(org-tag ((,class (:bold t :weight bold))))
     `(org-time-grid ((,class (:foreground ,cyan))))
     `(org-todo ((,class (:bold t :foreground ,red :weight bold))))
     `(org-upcoming-deadline ((,class (:bold t :foreground ,yellow ))))
     `(org-warning ((,class (:bold t :foreground ,orange :weight bold :underline t))))
     ;; org-habit (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
     `(org-habit-clear-face ((,class (:background ,blue-lc :foreground ,blue-hc))))
     `(org-habit-clear-future-face ((,class (:background ,blue-lc))))
     `(org-habit-ready-face ((,class (:background ,green-lc :foreground ,green))))
     `(org-habit-ready-future-face ((,class (:background ,green-lc))))
     `(org-habit-alert-face ((,class (:background ,yellow :foreground ,yellow-lc))))
     `(org-habit-alert-future-face ((,class (:background ,yellow-lc))))
     `(org-habit-overdue-face ((,class (:background ,red :foreground ,red-lc))))
     `(org-habit-overdue-future-face ((,class (:background ,red-lc))))

     ;; outline
     `(outline-8 ((,class (:inherit default))))
     `(outline-7 ((,class (:inherit outline-8 :height 1.0))))
     `(outline-6 ((,class (:inherit outline-7 :height 1.0))))
     `(outline-5 ((,class (:inherit outline-6 :height 1.0))))
     `(outline-4 ((,class (:inherit outline-5 :height 1.0))))
     `(outline-3 ((,class (:inherit outline-4 :height 1.0))))
     `(outline-2 ((,class (:inherit outline-3 :height 1.0))))
     `(outline-1 ((,class (:inherit outline-2 :height 1.0))))

     ;; rainbow-delimiters
     `(rainbow-delimiters-depth-1-face ((,class (:foreground ,cyan))))
     `(rainbow-delimiters-depth-2-face ((,class (:foreground ,yellow))))
     `(rainbow-delimiters-depth-3-face ((,class (:foreground ,blue))))
     `(rainbow-delimiters-depth-4-face ((,class (:foreground ,red))))
     `(rainbow-delimiters-depth-5-face ((,class (:foreground ,orange))))
     `(rainbow-delimiters-depth-6-face ((,class (:foreground ,blue))))
     `(rainbow-delimiters-depth-7-face ((,class (:foreground ,green))))
     `(rainbow-delimiters-depth-8-face ((,class (:foreground ,red))))
     `(rainbow-delimiters-depth-9-face ((,class (:foreground ,yellow))))
     `(rainbow-delimiters-depth-10-face ((,class (:foreground ,green))))
     `(rainbow-delimiters-depth-11-face ((,class (:foreground ,blue))))
     `(rainbow-delimiters-depth-12-face ((,class (:foreground ,red))))
     `(rainbow-delimiters-unmatched-face
       ((,class (:foreground ,solarized-fg :background ,solarized-bg :inverse-video t))))

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
     `(sh-quoted-exec ((,class (:foreground ,violet :weigth bold :bold t))))
     `(sh-escaped-newline ((,class (:foreground ,yellow :weigth bold :bold t))))
     `(sh-heredoc ((,class (:foreground ,yellow :weigth bold :bold t))))

     ;; show-paren
     `(show-paren-match
       ((,class (:foreground ,cyan :background ,solarized-bg :weight normal :inverse-video t))))
     `(show-paren-mismatch
       ((,class (:foreground ,red :background ,solarized-bg :weight normal :inverse-video t))))

     ;; SLIME
     `(slime-repl-inputed-output-face ((,class (:foreground ,red))))

     ;; undo-tree
     `(undo-tree-visualizer-default-face
       ((,class (:foreground ,solarized-comments :background ,solarized-bg))))
     `(undo-tree-visualizer-current-face ((,class (:foreground ,cyan :inverse-video t))))
     `(undo-tree-visualizer-active-branch-face
       ((,class (:foreground ,solarized-emph :background ,solarized-bg :weigth bold))))
     `(undo-tree-visualizer-register-face ((,class (:foreground ,yellow))))

     ;; table
     `(table-cell ((,class (:foreground ,solarized-fg :background ,solarized-hl))))

     ;; tooltip. (NOTE: This setting has no effect on the os widgets for me
     ;; zencoding uses this)
     `(tooltip ((,class (:background ,yellow-lc :foreground ,yellow-hc
                                     :inherit variable-pitch))))
     ;; volatile highlights
     `(vhl/default-face ((,class (:background ,green-lc :foreground ,green-hc))))

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

     ;; yascroll
     `(yascroll:thumb-text-area
       ((,class (:foreground ,solarized-comments :background ,solarized-comments))))
     `(yascroll:thumb-fringe
       ((,class (:foreground ,solarized-comments :background ,solarized-comments)))))

    ;; zencoding
    `(zencoding-preview-input ((,class (:background ,solarized-hl :box ,solarized-emph ))))

    (custom-theme-set-variables
     theme-name
     '(ansi-color-names-vector [solarized-bg red green yellow
                                             blue magenta cyan solarized-fg])

     ;; fill-column-indicator
     `(fci-rule-color ,solarized-hl))

    ;; call chained theme function
    (when childtheme (funcall childtheme))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'solarized-theme)

;;; solarized-theme.el ends here.
