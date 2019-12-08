(require 'solarized)
(require 'solarized-light-theme)
(require 'my-solarized)

;; This files needs to be places iside the custom-theme-load-path list

(deftheme my-solarized-light "The light variant of the Solarized colour theme")
(solarized-with-color-variables
  'light 'my-solarized-light solarized-light-color-palette-alist my-solarized-theme)

(provide-theme 'my-solarized-light)
