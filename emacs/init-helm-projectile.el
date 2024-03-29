(use-package helm
  :ensure t
  :config
  (helm-mode 1))

;; Projectile
;; https://docs.projectile.mx/projectile/installation.html#installation-via-package-el
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  (setq projectile-completion-system 'helm)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c j" . projectile-command-map))
  )

;;; helm-projectile
(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

;;; imenu
(use-package imenu-anywhere
  :ensure t)

;;; Global Key bindings
(global-set-key (kbd "C-.") 'helm-imenu-anywhere)


(global-set-key (kbd "C-c g a") 'helm-projectile-ack)

(provide 'init-helm-projectile)

