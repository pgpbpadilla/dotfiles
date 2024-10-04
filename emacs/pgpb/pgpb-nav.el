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

(defun pgpb-make-candidate (frame repo)
  "Generate frame candidates for interactive selection with Ivy."
  (let ((display-name (format "%s: [git: %s]"
          (frame-parameter frame 'name)
          (file-name-nondirectory (directory-file-name repo)))))
    (cons display-name frame)))

(defun pgpb-active-buffer-in-frame (frame)
  "Return the name of the active buffer in the given frame."
  (window-buffer (frame-selected-window frame)))

(defun pgpb-switch-frame ()
  "Switch to another frame interactively."
  (interactive)
  (ivy-read "Switch to frame: "
            (mapcar (lambda (frame)
                      (let* ((buffer (pgpb-active-buffer-in-frame frame))
                             (repo (or (pgpb-vc-root buffer) "No Git Repo")))
                        (pgpb-make-candidate frame repo)))
                    (frame-list))
            :action (lambda (candidate)
                      (select-frame-set-input-focus (cdr candidate)))))

(provide 'pgpb-nav)
