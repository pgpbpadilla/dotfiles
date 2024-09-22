(use-package helm
  :ensure t
  :config
  (helm-mode 1))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  (setq projectile-completion-system 'helm)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c j" . projectile-command-map))
  )

(use-package imenu-anywhere
  :ensure t
  :bind (("C-." . 'helm-imenu-anywhere)))

(use-package helm-projectile
  :ensure t
  :bind (("C-c g a" . 'helm-projectile-ack))
  :config
  (helm-projectile-on))

(provide 'pgpb-nav)
