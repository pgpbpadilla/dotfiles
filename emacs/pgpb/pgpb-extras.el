(use-package org-auto-tangle
  :ensure t
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(require 'pgpb-ts)

(defun pgpb-reinit ()
  "Reload the Emacs configuration."
  (interactive)
  (load-file user-init-file))

;; Bind the function to a key (optional)
(global-set-key (kbd "C-c r") 'pgpb-reinit)

(provide 'pgpb-extras)
