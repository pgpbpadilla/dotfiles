(use-package helm
  :config
  (require 'helm-config))

(use-package helm-mode
  :config
  (helm-mode 1))

;;; helm-projectile
(use-package helm-projectile
  :config
  (helm-projectile-on))

(use-package imenu-anywhere)

;;; Global Key bindings
(global-set-key (kbd "C-.") 'helm-imenu-anywhere)

(provide 'init-helm)

