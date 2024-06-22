(use-package org-auto-tangle
  :ensure t
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(require 'pgpb-ts)

(provide 'pgpb-extras)
