(require 'solarized-dev)

(global-set-key (kbd "<f4>") 'dev-disable-all-themes)
(global-set-key (kbd "<f5>") 'dev-reload-theme)
(global-set-key (kbd "<f6>") 'dev-cycle-main-themes)
(global-set-key (kbd "<f7>") 'dev-cycle-themes)
(global-set-key (kbd "<f8>") 'load-theme)
(global-set-key (kbd "<f9>") ' dev-switch-buffer-solarized)
(global-set-key (kbd "<f10>") 'dev-switch-buffer-current-theme)
(global-set-key (kbd "<f11>") 'dev-tf-switch-buffer-next)
(global-set-key (kbd "<f12>") 'dev-tf-switch-buffer-prev)

(provide 'solarized-dev-keys)
