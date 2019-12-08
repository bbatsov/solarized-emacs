(require 'solarized)
(require 'solarized-dark-theme)
(require 'my-solarized)

;; This files needs to be places iside the custom-theme-load-path list

(deftheme my-solarized-dark "The dark variant of the Solarized colour theme")
(solarized-with-color-variables
  'dark 'my-solarized-dark solarized-dark-color-palette-alist my-solarized-theme)

(provide-theme 'my-solarized-dark)
