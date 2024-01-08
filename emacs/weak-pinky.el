(keymap-global-set "M-J" 'next-line)
(keymap-global-set "M-I" 'previous-line)

(add-hook 'magit-status-mode
          (lambda ()
            (keymap-set magit-mode-map "M-e"
                        'magit-section-backward)
            (keymap-set magit-mode-map "M-f"
                        'magit-section-forward)))

(add-hook 'magit-status-mode
          (lambda ()
            (keymap-set magit-mode-map "M-e"
                        'magit-section-backward)
            (keymap-set magit-mode-map "M-f"
                        'magit-section-forward)))

(provide 'weak-pinky)
