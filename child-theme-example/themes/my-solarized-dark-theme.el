(require 'solarized)
(require 'my-solarized)

;; This files needs to be places iside the custom-theme-load-path list

(deftheme my-solarized-dark "The light variant of the Solarized colour theme")
(create-solarized-theme 'dark 'my-solarized-dark 'my-solarized-theme)

(provide-theme 'my-solarized-dark)
