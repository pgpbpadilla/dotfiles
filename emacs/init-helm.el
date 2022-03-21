(use-package helm
  :ensure t
  :config
  (require 'helm-config)
  (helm-mode 1))


;;; Global Key bindings
(global-set-key (kbd "C-.") 'helm-imenu-anywhere)


(provide 'init-helm)

