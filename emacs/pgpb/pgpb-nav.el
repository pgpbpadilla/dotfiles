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
  :bind ("C-c f" . pgpb-switch-frame)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d ")
  (enable-recursive-minibuffers t))

(defun pgpb-vc-root (buffer-name)
  "Return the name of the parent git repo for the given BUFFER-NAME."
  (with-current-buffer buffer-name
    (let ((git-root (vc-root-dir)))
      (if git-root
          (file-name-nondirectory (directory-file-name git-root))
        "Not in a git repo"))))

(defun pgpb-switch-frame ()
  (interactive)
  (ivy-read "Switch to frame: "
            (mapcar (lambda (frame)
                      (let* ((buffer (window-buffer (frame-selected-window frame)))
                             (repo (or (pgpb-vc-root buffer) "No Git Repo")))
                        (cons (format "%s - [git: %s]"
                                      (frame-parameter frame 'name)
                                      (file-name-nondirectory (directory-file-name repo))) frame)))
                    (frame-list))
            :action (lambda (frame)
                      (select-frame-set-input-focus frame))))

(provide 'pgpb-nav)
