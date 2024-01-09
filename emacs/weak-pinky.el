(keymap-global-set "M-J" 'next-line)
(keymap-global-set "M-I" 'previous-line)

(add-hook 'magit-status-mode
          (lambda ()
            (keymap-set magit-mode-map "M-e"
                        'magit-section-backward)
            (keymap-set magit-mode-map "M-f"
                        'magit-section-forward)))

(use-package magit
  :ensure t
  :bind (:map magit-mode-map
              ("C-S" . magit-section-backward)
              ("C-D" . magit-section-forward)
              ("M-S" . magit-section-backward-sibling)
              ("M-D" . magit-section-forward-sibling)))

(provide 'weak-pinky)
