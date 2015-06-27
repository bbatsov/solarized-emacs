(require 'solarized)
(require 'my-solarized)

;; This files needs to be places iside the custom-theme-load-path list

(deftheme my-solarized-light "The light variant of the Solarized colour theme")
(create-solarized-theme 'light 'my-solarized-light 'my-solarized-theme)

(provide-theme 'my-solarized-light)
