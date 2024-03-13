(keymap-global-set "M-J" 'next-line)
(keymap-global-set "M-I" 'previous-line)

(keymap-global-set "C-<escape>" 'undo)

(use-package magit
  :ensure t
  :bind (:map magit-mode-map
              ("C-S" . magit-section-backward)
              ("C-D" . magit-section-forward)
              ("M-S" . magit-section-backward-sibling)
              ("M-D" . magit-section-forward-sibling)))

(provide 'weak-pinky)
