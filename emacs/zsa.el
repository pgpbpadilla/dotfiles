(defun zsa-start ()
  "Focus on task and clock"
  (interactive)
  (funcall (lookup-key (current-local-map) (kbd "RET")))
  (org-narrow-to-subtree)
  (delete-other-windows)
  (org-clock-in)
)

(global-set-key (kbd "C-c u") 'zsa-start)

(provide 'zsa)


