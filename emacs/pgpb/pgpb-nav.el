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

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  :bind ("C-c f" . ivy-switch-frame)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d ")
  (enable-recursive-minibuffers t))

(defun ivy-switch-frame ()
  "Use Ivy to switch between visible Emacs frames."
  (interactive)
  (let ((frames (mapcar (lambda (frame)
                          (cons (frame-parameter frame 'name) frame))
                        (frame-list))))
    (ivy-read "Switch to frame: "
              (mapcar 'car frames)
              :action (lambda (name)
                        (select-frame-set-input-focus
                         (cdr (assoc name frames)))))))

(provide 'pgpb-nav)
